program main
    use :: raylib
    use :: follow_camera_mod
    use, intrinsic :: iso_c_binding, only: c_null_char, c_null_ptr, c_ptr, c_f_pointer, c_int

    implicit none (type, external)

    type(model_type) :: hero_model
    real             :: hero_scale

    integer, parameter :: SCREEN_WIDTH  = 800
    integer, parameter :: SCREEN_HEIGHT = 600
    integer, parameter :: GRID_SIZE = 16
    integer, parameter :: MAX_LAYERS = 8
    real, parameter    :: CELL_SIZE = 1.0
    real, parameter    :: CELL_HEIGHT = 1.0

    integer, parameter :: FLOOR = 1

    integer :: layers_count(0:GRID_SIZE-1, 0:GRID_SIZE-1) = 0
    integer :: map_encoded(0:GRID_SIZE-1, 0:GRID_SIZE-1, MAX_LAYERS) = 0

    type(FollowCamera)  :: follow_cam
    type(camera3d_type) :: camera
    type(vector3_type)  :: player_pos, forward

    real :: dt, rot_speed = 2.5, player_speed = 2.5
    real :: camera_distance = 5.0, camera_height = 1.0
    real :: move_angle_h = 0.0
    real :: player_dir_angle = 0.0
    real :: model_angle

    ! ---------- Анимации ----------
    type(c_ptr) :: anims_ptr = c_null_ptr
    integer(kind=c_int) :: anim_count = 0
    logical :: have_anim = .false.
    integer :: anim_index = 1
    type(model_animation_type), pointer, dimension(:) :: anims => null()

    real :: anim_timer = 0.0
    integer :: current_frame = 0

    integer :: player_ix, player_iz
    logical :: moving = .false.
    integer :: target_ix, target_iz
    real :: target_x, target_z, target_y
    real :: dx, dy, dz, dist, step

    ! ---------- Для подгонки размера модели ----------
    type(bounding_box_type) :: hero_box
    real                    :: hero_height, hero_min_y

    type(color_type) :: palette(0:7)

    character(len=256) :: map_path, line, cell_str, part
    integer :: i, j, k, ios, pos, comma_pos, encoded, layer_type, layer_color

    ! ====================== ИНИЦИАЛИЗАЦИЯ ======================
    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Fortran RPG - 3D Hero' // c_null_char)
    call set_target_fps(60)

    hero_model = load_model('assets/allies/models/hero.glb' // c_null_char)

    ! Узнаём реальный размер модели и подгоняем масштаб
    hero_box    = get_model_bounding_box(hero_model)
    hero_height = hero_box%max%y - hero_box%min%y
    hero_min_y  = hero_box%min%y
    print*, 'Original hero height:', hero_height
    hero_scale = 1.5 / hero_height          ! 1.5 – желаемый рост (можно менять)

    ! Загрузка анимаций
    anims_ptr = load_model_animations('assets/allies/models/hero.glb' // c_null_char, anim_count)
    if (anim_count > 0) then
        call c_f_pointer(anims_ptr, anims, [anim_count])
        have_anim = .true.
        print*, '=== ANIMATIONS LOADED ==='
        print*, 'Total animations:', anim_count
        do i = 1, min(anim_count, 20)
            print*, '  ', i, ' -> ', anims(i)%name
        end do
        print*, '========================'
    end if

    palette(0) = GRAY
    palette(1) = RED
    palette(2) = GREEN
    palette(3) = BLUE
    palette(4) = YELLOW
    palette(5) = ORANGE
    palette(6) = PURPLE
    palette(7) = PINK

    ! ====================== ЗАГРУЗКА КАРТЫ ======================
    map_path = 'data/maps/576.map'
    open(unit=10, file=map_path, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print*, 'Map file not found. Creating demo room.'
        do i = 7, 9
            do j = 7, 9
                layers_count(i,j) = 1
                map_encoded(i,j,1) = FLOOR * 8 + 0
            end do
        end do
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

    ! ====================== СТАРТОВАЯ ПОЗИЦИЯ ======================
    player_ix = -1
    player_iz = -1
    do i = 0, GRID_SIZE-1
        do j = 0, GRID_SIZE-1
            if (layers_count(i,j) > 0) then
                encoded = map_encoded(i,j, layers_count(i,j))
                layer_type = encoded / 8
                if (layer_type == FLOOR) then
                    player_ix = i
                    player_iz = j
                    exit
                end if
            end if
        end do
        if (player_ix /= -1) exit
    end do

    if (player_ix == -1) then
        print*, 'No walkable cell found. Exiting.'
        call close_window()
        stop
    end if

    ! Ноги ставятся на верхнюю грань самого верхнего куба в клетке
    player_pos = vector3_type(real(player_ix) + 0.5, &
                              real(layers_count(player_ix, player_iz)) * CELL_HEIGHT &
                              - hero_min_y * hero_scale, &
                              real(player_iz) + 0.5)

    target_ix = player_ix
    target_iz = player_iz
    call init_follow_camera(follow_cam, player_pos, camera_distance, camera_height)

    ! ====================== ГЛАВНЫЙ ЦИКЛ ======================
    do while (.not. window_should_close())
        dt = get_frame_time()

        ! ---------- АНИМАЦИЯ ----------
        if (have_anim .and. anim_count > 0) then
            if (moving) then
                if (anim_index /= 11) then      ! Robot_Walking (обычная ходьба)
                    anim_index = 11
                    anim_timer = 0.0
                end if
            else
                if (anim_index /= 3) then       ! Robot_Idle (стоя, голова кивает)
                    anim_index = 3
                    anim_timer = 0.0
                end if
            end if

            if (anim_index < 1 .or. anim_index > anim_count) anim_index = 3

            call update_model_animation(hero_model, anims(anim_index), current_frame)

            ! Скорость подогнана так, чтобы один цикл ходьбы (57 кадров) занимал
            ! ровно столько же времени, сколько персонаж идёт одну клетку (при player_speed = 2.5)
            anim_timer = anim_timer + dt * 110.5
            current_frame = mod(int(anim_timer), anims(anim_index)%frame_count)
        end if

        ! ---------- ПОВОРОТ ----------
        if (is_key_down(KEY_LEFT))  move_angle_h = move_angle_h - rot_speed * dt
        if (is_key_down(KEY_RIGHT)) move_angle_h = move_angle_h + rot_speed * dt

        call update_follow_camera(follow_cam, player_pos, move_angle_h)

        forward%x = sin(move_angle_h)
        forward%y = 0.0
        forward%z = -cos(move_angle_h)

        ! ---------- ДВИЖЕНИЕ ----------
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
                    if (layers_count(target_ix, target_iz) > 0) then
                        encoded = map_encoded(target_ix, target_iz, layers_count(target_ix, target_iz))
                        if (encoded / 8 == FLOOR) then
                            moving = .true.
                            target_x = real(target_ix) + 0.5
                            target_z = real(target_iz) + 0.5
                            target_y = real(layers_count(target_ix, target_iz)) * CELL_HEIGHT &
                                       - hero_min_y * hero_scale
                        end if
                    end if
                end if
            end if
        end if

        if (moving) then
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
                moving = .false.
            else
                player_pos%x = player_pos%x + dx / dist * step
                player_pos%z = player_pos%z + dz / dist * step
                player_pos%y = player_pos%y + dy / dist * step
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
                                layer_type = encoded / 8
                                layer_color = mod(encoded, 8)
                                if (layer_type >= 1 .and. layer_type <= 3) then
                                    call draw_cube(vector3_type(real(i)+0.5, &
                                                  real(k-1)*CELL_HEIGHT+0.5, real(j)+0.5), &
                                                  CELL_SIZE, CELL_HEIGHT, CELL_SIZE, &
                                                  palette(layer_color))
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

            call draw_text('LEFT/RIGHT - turn | UP/DOWN - move' // c_null_char, 10, 10, 20, DARKGRAY)
            call draw_fps(700, 10)
        call end_drawing()
    end do

    ! ====================== ОЧИСТКА ======================
    if (have_anim) then
        call unload_model_animations(anims, anim_count)
    end if
    call unload_model(hero_model)
    call close_window()

end program main