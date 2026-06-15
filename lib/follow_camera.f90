module follow_camera_mod
    use :: raylib
    implicit none (type, external)
    public

    type, public :: FollowCamera
        type(camera3d_type) :: cam
        real :: distance
        real :: height          ! будет пересчитываться автоматически
        real :: height_ratio    ! height / distance (сохраняет угол наклона)
        real :: target_offset_y
        real :: min_dist
        real :: max_dist
        real :: zoom_speed
    end type FollowCamera

contains

    subroutine init_follow_camera(self, target_pos, distance, height)
        type(FollowCamera), intent(inout) :: self
        type(vector3_type), intent(in)    :: target_pos
        real, intent(in)                 :: distance, height

        self%distance        = distance
        self%height          = height
        self%height_ratio    = height / distance   ! запоминаем пропорцию
        self%target_offset_y = 0.5
        self%min_dist        = 3.0
        self%max_dist        = 25.0
        self%zoom_speed      = 1.0

        self%cam%up         = vector3_type(0.0, 1.0, 0.0)
        self%cam%fov_y      = 60.0
        self%cam%projection = CAMERA_PERSPECTIVE

        ! Начальная позиция камеры (сразу за спиной, angle=0)
        self%cam%target = vector3_type(target_pos%x, target_pos%y + self%target_offset_y, target_pos%z)
        self%cam%position%x = target_pos%x
        self%cam%position%y = target_pos%y + self%height
        self%cam%position%z = target_pos%z + self%distance
    end subroutine init_follow_camera

    subroutine update_follow_camera(self, target_pos, angle_h)
        type(FollowCamera), intent(inout) :: self
        type(vector3_type), intent(in)    :: target_pos
        real, intent(in)                 :: angle_h    ! угол направления движения (радианы)

        real :: forward_x, forward_z

        ! Обработка зума колёсиком мыши
        self%distance = self%distance - get_mouse_wheel_move() * self%zoom_speed
        if (self%distance < self%min_dist) self%distance = self%min_dist
        if (self%distance > self%max_dist) self%distance = self%max_dist

        ! Высота автоматически подстраивается, чтобы сохранить угол обзора
        self%height = self%distance * self%height_ratio

        ! Вектор "вперёд" от угла
        forward_x = sin(angle_h)
        forward_z = -cos(angle_h)

        ! Камера сзади и сверху
        self%cam%target = vector3_type(target_pos%x, target_pos%y + self%target_offset_y, target_pos%z)
        self%cam%position%x = target_pos%x - forward_x * self%distance
        self%cam%position%y = target_pos%y + self%height
        self%cam%position%z = target_pos%z - forward_z * self%distance
    end subroutine update_follow_camera

end module follow_camera_mod