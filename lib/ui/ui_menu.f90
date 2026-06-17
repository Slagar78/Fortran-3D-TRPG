module ui_menu_mod
    use :: raylib
    use, intrinsic :: iso_c_binding, only: c_null_char
    implicit none
    private

    public :: menu_type
    public :: init_menu, update_menu, draw_menu, destroy_menu

    type :: menu_type
        type(texture2d_type) :: tex(4)
        type(rectangle_type) :: rect(4)
        character(len=10)    :: labels(4)
        integer :: btn_w = 60, btn_h = 60
        logical :: visible = .false.
        integer :: selected = 1
    end type menu_type

contains

    subroutine init_menu(menu, screen_w, screen_h)
        type(menu_type), intent(inout) :: menu
        integer, intent(in) :: screen_w, screen_h
        integer :: center_x, center_y, i
        character(len=64) :: files(4)

        files(1) = "assets/buttons/attack.png" // c_null_char
        files(2) = "assets/buttons/magic.png"  // c_null_char
        files(3) = "assets/buttons/item.png"   // c_null_char
        files(4) = "assets/buttons/stay.png"   // c_null_char

        menu%labels(1) = "Attack"
        menu%labels(2) = "Magic"
        menu%labels(3) = "Item"
        menu%labels(4) = "Stay"

        do i = 1, 4
            menu%tex(i) = load_texture(files(i))
        end do

        center_x = screen_w / 2
        center_y = screen_h - 120

        menu%rect(1) = rectangle_type( &
            real(center_x - menu%btn_w/2), &
            real(center_y - menu%btn_h), &
            real(menu%btn_w), real(menu%btn_h) )

        menu%rect(2) = rectangle_type( &
            real(center_x - menu%btn_w - menu%btn_w/2), &
            real(center_y - menu%btn_h/2), &
            real(menu%btn_w), real(menu%btn_h) )

        menu%rect(3) = rectangle_type( &
            real(center_x + menu%btn_w/2), &
            real(center_y - menu%btn_h/2), &
            real(menu%btn_w), real(menu%btn_h) )

        menu%rect(4) = rectangle_type( &
            real(center_x - menu%btn_w/2), &
            real(center_y), &
            real(menu%btn_w), real(menu%btn_h) )

        menu%visible = .false.
        menu%selected = 1
    end subroutine init_menu

    function update_menu(menu) result(btn)
        type(menu_type), intent(inout) :: menu
        integer :: btn
        integer :: i
        type(vector2_type) :: mouse

        btn = 0

        if (is_key_pressed(KEY_A) .or. is_key_pressed(KEY_D)) then
            menu%visible = .not. menu%visible
            if (menu%visible) menu%selected = 1
            return
        end if

        if (is_key_pressed(KEY_S)) then
            menu%visible = .false.
            return
        end if

        if (.not. menu%visible) return

        if (is_key_pressed(KEY_UP))    menu%selected = 1
        if (is_key_pressed(KEY_LEFT))  menu%selected = 2
        if (is_key_pressed(KEY_RIGHT)) menu%selected = 3
        if (is_key_pressed(KEY_DOWN))  menu%selected = 4

        if (is_key_pressed(KEY_ENTER) .or. is_key_pressed(KEY_SPACE)) then
            btn = menu%selected
            menu%visible = .false.
            return
        end if

        if (is_mouse_button_pressed(MOUSE_BUTTON_LEFT)) then
            mouse = get_mouse_position()
            do i = 1, 4
                if (check_collision_point_rec(mouse, menu%rect(i))) then
                    btn = i
                    menu%visible = .false.
                    return
                end if
            end do
        end if

        mouse = get_mouse_position()
        do i = 1, 4
            if (check_collision_point_rec(mouse, menu%rect(i))) then
                menu%selected = i
                exit
            end if
        end do
    end function update_menu

    subroutine draw_menu(menu)
        type(menu_type), intent(in) :: menu
        integer :: i
        real :: text_x, text_y
        character(len=20) :: label_text

        if (.not. menu%visible) return

        do i = 1, 4
            call draw_texture_pro(menu%tex(i), &
                rectangle_type(0.0, 0.0, real(menu%btn_w), real(menu%btn_h)), &
                menu%rect(i), &
                vector2_type(0.0, 0.0), &
                0.0, &
                WHITE)

            if (i == menu%selected) then
                call draw_rectangle_lines_ex(menu%rect(i), 2.0, RED)
            else if (check_collision_point_rec(get_mouse_position(), menu%rect(i))) then
                call draw_rectangle_lines_ex(menu%rect(i), 2.0, YELLOW)
            end if
        end do

        ! Текст всегда справа от правой кнопки (индекс 3)
        text_x = menu%rect(3)%x + menu%rect(3)%width + 40.0
        text_y = menu%rect(3)%y + (menu%rect(3)%height - 40.0) / 2.0
        label_text = trim(menu%labels(menu%selected)) // c_null_char
        call draw_text(label_text, int(text_x), int(text_y), 40, BLACK)
    end subroutine draw_menu

    subroutine destroy_menu(menu)
        type(menu_type), intent(inout) :: menu
        integer :: i

        do i = 1, 4
            call unload_texture(menu%tex(i))
        end do
    end subroutine destroy_menu

end module ui_menu_mod