program main
    use :: raylib
    use :: follow_camera_mod
    use, intrinsic :: iso_c_binding, only: c_null_char, c_null_ptr, c_ptr, c_f_pointer, c_int
    use :: ui_menu_mod

    implicit none (type, external)

    type(model_type) :: hero_model
    real             :: hero_scale

    integer, parameter :: SCREEN_WIDTH  = 800
    integer, parameter :: SCREEN_HEIGHT = 600
    integer, parameter :: GRID_SIZE = 16
    integer, parameter :: MAX_LAYERS = 8
    real, parameter    :: CELL_SIZE = 1.0
    real, parameter    :: CELL_HEIGHT = 1.0

    integer :: layers_count(0:GRID_SIZE-1, 0:GRID_SIZE-1) = 0
    integer :: map_encoded(0:GRID_SIZE-1, 0:GRID_SIZE-1, MAX_LAYERS) = 0

    type(FollowCamera)  :: follow_cam
    type(camera3d_type) :: camera
    type(vector3_type)  :: player_pos, forward

    real :: dt, rot_speed = 2.5, player_speed = 3.0
    real :: camera_distance = 5.0, camera_height = 1.0
    real :: move_angle_h = 0.0
    real :: player_dir_angle = 0.0
    real :: model_angle

    ! ---------- Физика прыжка ----------
    real, parameter :: GRAVITY = 10.0
    real, parameter :: JUMP_VELOCITY = 5.5
    real :: vertical_vel = 0.0
    logical :: is_grounded = .true.

    ! ---------- Анимации ----------
    type(c_ptr) :: anims_ptr = c_null_ptr
    integer(kind=c_int) :: anim_count = 0
    logical :: have_anim = .false.
    integer :: anim_index = 3
    type(model_animation_type), pointer, dimension(:) :: anims => null()

    real :: anim_timer = 0.0
    integer :: current_frame = 0

    integer :: player_ix, player_iz, player_layer
    type(menu_type) :: action_menu
    logical :: moving = .false.
    ! для движения по земле
    integer :: target_ix, target_iz, target_layer
    real :: target_x, target_z, target_y
    real :: dx, dy, dz, dist, step

    type(bounding_box_type) :: hero_box
    real                    :: hero_height, hero_min_y

    type(color_type) :: palette(0:7)

    character(len=256) :: map_path, line, cell_str, part
    integer :: i, j, k, ios, pos, comma_pos, encoded, layer_type, layer_color, rotation

    ! ---------- Музыка ----------
    type(music_type) :: bgm
    logical          :: music_ready = .false.

    ! ====================== ИНИЦИАЛИЗАЦИЯ ======================
    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran RPG - 3D Hero' // c_null_char)
    call set_target_fps(60)
    call init_audio_device()

    hero_model = load_model('assets/allies/models/hero.glb' // c_null_char)

    hero_box    = get_model_bounding_box(hero_model)
    hero_height = hero_box%max%y - hero_box%min%y
    hero_min_y  = hero_box%min%y
    hero_scale = 1.5 / hero_height

    anims_ptr = load_model_animations('assets/allies/models/hero.glb' // c_null_char, anim_count)
    if (anim_count > 0) then
        call c_f_pointer(anims_ptr, anims, [anim_count])
        have_anim = .true.
    end if

    palette(0) = GRAY
    palette(1) = RED
    palette(2) = GREEN
    palette(3) = BLUE
    palette(4) = YELLOW
    palette(5) = ORANGE
    palette(6) = PURPLE
    palette(7) = PINK

    bgm = load_music_stream('assets/sounds/Brave_Eagle.mp3' // c_null_char)
    music_ready = .true.
    call set_music_volume(bgm, 0.5)
    call play_music_stream(bgm)

    ! ---------- Инициализация меню ----------
    call init_menu(action_menu, SCREEN_WIDTH, SCREEN_HEIGHT)

    map_path = 'data/maps/576.map'
    open(unit=10, file=map_path, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print*, 'Map file not found. Using empty map.'
    else
        do i = 0, GRID_SIZE-1
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            pos = 1
            do j = 0, GRID_SIZE-1
                cell_str = ''
                do while (pos <= len_trim(line) .and. line(pos:pos) /= ' ')
                    cell_str = trim(cell_str) // line(pos:pos)
                    pos = pos + 1
                end do
                pos = pos + 1
                if (cell_str == '0') then
                    layers_count(i,j) = 0
                else
                    layers_count(i,j) = 0
                    do while (len_trim(cell_str) > 0)
                        comma_pos = index(cell_str, ',')
                        if (comma_pos == 0) then
                            part = cell_str
                            cell_str = ''
                        else
                            part = cell_str(1:comma_pos-1)
                            cell_str = cell_str(comma_pos+1:)
                        end if
                        read(part, *) encoded
                        layers_count(i,j) = layers_count(i,j) + 1
                        map_encoded(i,j, layers_count(i,j)) = encoded
                    end do
                end if
            end do
        end do
        close(10)
    end if

    player_ix = GRID_SIZE/2
    player_iz = GRID_SIZE/2
    do i = 0, GRID_SIZE-1
        do j = 0, GRID_SIZE-1
            if (layers_count(i,j) == 0) then
                player_ix = i
                player_iz = j
                exit
            end if
        end do
        if (layers_count(player_ix,player_iz) == 0) exit
    end do
    player_layer = 0
    player_pos = vector3_type(real(player_ix) + 0.5, &
                              real(player_layer) * CELL_HEIGHT - hero_min_y * hero_scale, &
                              real(player_iz) + 0.5)
    call init_follow_camera(follow_cam, player_pos, camera_distance, camera_height)

    ! ====================== ГЛАВНЫЙ ЦИКЛ ======================
    do while (.not. window_should_close())
        dt = get_frame_time()

       if (music_ready) call update_music_stream(bgm)
	   
    ! ---------- СБРОС moving ПРИ ОТКРЫТОМ МЕНЮ ----------
    if (action_menu%visible) then
      moving = .false.
        end if
        ! ---------- АНИМАЦИЯ ----------
        if (have_anim .and. anim_count > 0) then
            if (.not. is_grounded) then
                anim_index = 4   ! Robot_Jump
            else if (moving) then
                anim_index = 11  ! Robot_Walking
            else
                anim_index = 3   ! Robot_Idle
            end if

            if (anim_index < 1 .or. anim_index > anim_count) anim_index = 3

            if (anim_index == 4) then
                if (anim_timer < real(anims(4)%frame_count)) then
                    current_frame = int(anim_timer)
                else
                    current_frame = anims(4)%frame_count - 1
                end if
            else
                current_frame = mod(int(anim_timer), anims(anim_index)%frame_count)
            end if

            call update_model_animation(hero_model, anims(anim_index), current_frame)
            anim_timer = anim_timer + dt * 142.5
        end if

        ! ---------- ПОВОРОТ (блокируется при открытом меню) ----------
        if (.not. action_menu%visible) then
            if (is_key_down(KEY_LEFT))  move_angle_h = move_angle_h - rot_speed * dt
            if (is_key_down(KEY_RIGHT)) move_angle_h = move_angle_h + rot_speed * dt
        end if

        call update_follow_camera(follow_cam, player_pos, move_angle_h)

        forward%x = sin(move_angle_h)
        forward%y = 0.0
        forward%z = -cos(move_angle_h)

        ! ---------- ДВИЖЕНИЕ И ПРЫЖКИ (блокируется при открытом меню) ----------
        if (.not. action_menu%visible) then
            if (is_grounded) then
                ! ---------- НА ЗЕМЛЕ ----------
                if (.not. moving) then
                    if (is_key_down(KEY_UP)) then
                        target_ix = player_ix + nint(forward%x)
                        target_iz = player_iz + nint(forward%z)
                        player_dir_angle = move_angle_h
                    else if (is_key_down(KEY_DOWN)) then
                        target_ix = player_ix - nint(forward%x)
                        target_iz = player_iz - nint(forward%z)
                        player_dir_angle = move_angle_h + 3.14159265
                    else
                        target_ix = player_ix
                        target_iz = player_iz
                    end if

                    if (target_ix /= player_ix .or. target_iz /= player_iz) then
                        if (target_ix >= 0 .and. target_ix < GRID_SIZE .and. &
                            target_iz >= 0 .and. target_iz < GRID_SIZE) then
                            target_layer = layers_count(target_ix, target_iz)
                            if (target_layer <= player_layer) then
                                moving = .true.
                                target_x = real(target_ix) + 0.5
                                target_z = real(target_iz) + 0.5
                                target_y = real(target_layer) * CELL_HEIGHT - hero_min_y * hero_scale
                            end if
                        end if
                    end if

                    if (is_key_pressed(KEY_SPACE)) then
                        is_grounded = .false.
                        vertical_vel = JUMP_VELOCITY
                        anim_timer = 0.0
                        moving = .true.
                    end if
                end if

                if (moving .and. is_grounded) then
                    dx = target_x - player_pos%x
                    dz = target_z - player_pos%z
                    dy = target_y - player_pos%y
                    dist = sqrt(dx*dx + dz*dz + dy*dy)
                    step = player_speed * dt

                    if (dist <= step) then
                        player_pos%x = target_x
                        player_pos%z = target_z
                        player_pos%y = target_y
                        player_ix = target_ix
                        player_iz = target_iz
                        player_layer = target_layer
                        moving = .false.
                    else
                        player_pos%x = player_pos%x + dx / dist * step
                        player_pos%z = player_pos%z + dz / dist * step
                        player_pos%y = player_pos%y + dy / dist * step
                    end if
                end if
            else
                ! ---------- В ВОЗДУХЕ ----------
                if (is_key_down(KEY_UP)) then
                    player_pos%x = player_pos%x + forward%x * player_speed * dt
                    player_pos%z = player_pos%z + forward%z * player_speed * dt
                else if (is_key_down(KEY_DOWN)) then
                    player_pos%x = player_pos%x - forward%x * player_speed * dt
                    player_pos%z = player_pos%z - forward%z * player_speed * dt
                end if

                player_ix = floor(player_pos%x)
                player_iz = floor(player_pos%z)

                if (player_ix < 0) player_ix = 0
                if (player_iz < 0) player_iz = 0
                if (player_ix >= GRID_SIZE) player_ix = GRID_SIZE-1
                if (player_iz >= GRID_SIZE) player_iz = GRID_SIZE-1

                if (layers_count(player_ix, player_iz) > player_layer) then
                    player_pos%x = player_pos%x - forward%x * player_speed * dt
                    player_pos%z = player_pos%z - forward%z * player_speed * dt
                    player_ix = floor(player_pos%x)
                    player_iz = floor(player_pos%z)
                end if

                vertical_vel = vertical_vel - GRAVITY * dt
                player_pos%y = player_pos%y + vertical_vel * dt

                target_y = real(layers_count(player_ix, player_iz)) * CELL_HEIGHT - hero_min_y * hero_scale
                if (player_pos%y <= target_y) then
                    player_pos%y = target_y
                    player_layer = layers_count(player_ix, player_iz)
                    is_grounded = .true.
                    moving = .false.
                    vertical_vel = 0.0
                    anim_timer = 0.0
                    player_pos%x = real(player_ix) + 0.5
                    player_pos%z = real(player_iz) + 0.5
                end if
            end if
        end if

        model_angle = -player_dir_angle * 57.29578 + 180.0

        ! ---------- ОТРИСОВКА ----------
        camera = follow_cam%cam
        call begin_drawing()
            call clear_background(RAYWHITE)
            call begin_mode3d(camera)

                do i = 0, GRID_SIZE
                    call draw_line3d(vector3_type(real(i), 0.0, 0.0), &
                                     vector3_type(real(i), 0.0, real(GRID_SIZE)), DARKGRAY)
                    call draw_line3d(vector3_type(0.0, 0.0, real(i)), &
                                     vector3_type(real(GRID_SIZE), 0.0, real(i)), DARKGRAY)
                end do

                do i = 0, GRID_SIZE-1
                    do j = 0, GRID_SIZE-1
                        if (layers_count(i,j) > 0) then
                            do k = 1, layers_count(i,j)
                                encoded = map_encoded(i,j,k)
                                if (encoded == 8) then
                                    layer_color = 0
                                    rotation = 0
                                else
                                    layer_color = iand(encoded, 15)
                                    rotation = iand(ishft(encoded, -4), 3)
                                end if
                                layer_type = merge(1, 2, layer_color == 0)

                                if (layer_type >= 1 .and. layer_type <= 2) then
                                    if (layer_color == 1) then
                                        call draw_triangle_prism(real(i)+0.5, &
                                            real(k-1)*CELL_HEIGHT+0.5, real(j)+0.5, &
                                            CELL_SIZE, CELL_HEIGHT, rotation, palette(1))
                                    else
                                        call draw_cube(vector3_type(real(i)+0.5, &
                                            real(k-1)*CELL_HEIGHT+0.5, real(j)+0.5), &
                                            CELL_SIZE, CELL_HEIGHT, CELL_SIZE, &
                                            palette(layer_color))
                                    end if
                                end if
                            end do
                        end if
                    end do
                end do

                call draw_model_ex(hero_model, player_pos, &
                                   vector3_type(0.0, 1.0, 0.0), &
                                   model_angle, &
                                   vector3_type(hero_scale, hero_scale, hero_scale), &
                                   WHITE)

            call end_mode3d()

            ! ---------- МЕНЮ ДЕЙСТВИЙ ----------
            select case (update_menu(action_menu))
            case (1)
                print*, "Attack!"
            case (2)
                print*, "Magic!"
            case (3)
                print*, "Item!"
            case (4)
                print*, "Stay!"
            end select
            call draw_menu(action_menu)

            call draw_text('LEFT/RIGHT - turn | UP/DOWN - move | SPACE - jump' // c_null_char, 10, 10, 20, DARKGRAY)
            call draw_fps(700, 10)
        call end_drawing()
    end do

    ! ====================== ОЧИСТКА ======================
    call destroy_menu(action_menu)
    if (music_ready) then
        call stop_music_stream(bgm)
        call unload_music_stream(bgm)
    end if
    if (have_anim) then
        call unload_model_animations(anims, anim_count)
    end if
    call unload_model(hero_model)
    call close_audio_device()
    call close_window()

contains

    subroutine draw_triangle_prism(cx, cy, cz, size_xz, height, rotation, color)
        use :: raylib
        implicit none
        real, intent(in) :: cx, cy, cz, size_xz, height
        integer, intent(in) :: rotation
        type(color_type), intent(in) :: color
        real :: half, h, angle, cos_a, sin_a
        type(vector3_type) :: pts(3), top(3), bot(3)
        integer :: i

        half = size_xz / 2.0
        h = height / 2.0

        pts(1) = vector3_type(-half, 0.0, -half)
        pts(2) = vector3_type( half, 0.0, -half)
        pts(3) = vector3_type(-half, 0.0,  half)

        angle = rotation * 3.14159265 / 2.0
        cos_a = cos(angle)
        sin_a = sin(angle)

        do i = 1, 3
            top(i)%x = pts(i)%x * cos_a - pts(i)%z * sin_a
            top(i)%y = pts(i)%y
            top(i)%z = pts(i)%x * sin_a + pts(i)%z * cos_a

            top(i)%x = top(i)%x + cx
            top(i)%y = top(i)%y + cy
            top(i)%z = top(i)%z + cz
        end do

        do i = 1, 3
            top(i) = vector3_type(top(i)%x, cy + h, top(i)%z)
            bot(i) = vector3_type(top(i)%x, cy - h, top(i)%z)
        end do

        call draw_triangle3d(top(1), top(2), top(3), color)
        call draw_triangle3d(bot(1), bot(3), bot(2), color)

        call draw_triangle3d(top(1), bot(1), top(2), color)
        call draw_triangle3d(top(2), bot(1), bot(2), color)
        call draw_triangle3d(top(2), bot(2), top(3), color)
        call draw_triangle3d(top(3), bot(2), bot(3), color)
        call draw_triangle3d(top(3), bot(3), top(1), color)
        call draw_triangle3d(top(1), bot(3), bot(1), color)

        call draw_line3d(top(1), top(2), BLACK)
        call draw_line3d(top(2), top(3), BLACK)
        call draw_line3d(top(3), top(1), BLACK)
        call draw_line3d(bot(1), bot(2), BLACK)
        call draw_line3d(bot(2), bot(3), BLACK)
        call draw_line3d(bot(3), bot(1), BLACK)
        call draw_line3d(top(1), bot(1), BLACK)
        call draw_line3d(top(2), bot(2), BLACK)
        call draw_line3d(top(3), bot(3), BLACK)
    end subroutine draw_triangle_prism

end program main