program main
    use machina_map
    use machina_vla
    use machina_basic
    use machina_core
    use machina_error
    implicit none
    type(map_t) :: dict
    type(vla_char), allocatable :: v1
    type(vla_int), allocatable :: v2
    type(vla_char), pointer :: vc_ptr
    type(error_t) :: error

    allocate (v1)
    call v1%push_back("1111")
    call v1%push_back("222")

    allocate (v2)
    call v2%push_back([1, 2, 3, 4, 5])

    call dict%insert("s1", 0)
    call dict%insert("v1", v1)
    call dict%insert("s2", .false.)
    call dict%insert("v2", v2)
    call dict%insert("s3", "fortran")
    call dict%insert("s4", 2.718_f8)

    call dict%get_vla_char("v1", vc_ptr, error)
    if (.not. .has.error) then
        block
            type(vla_char_const_iterator) :: it
            it = vc_ptr%const_iterator()

            do while (it%has_next())
                print *, it%get_next()
            end do
        end block
    end if

    call dict%remove("v1")

    block
        type(map_iterator) :: it
        class(machina_value), pointer :: ptr
        character(len=:), allocatable :: key
        it = dict%iterator()

        do while (it%has_next())
            call it%next_pair(key, ptr)
            select type (ptr)
            type is (int_value)
                write (*, "('key=',A,' value=',g0)") key, ptr%raw
            type is (bool_value)
                write (*, "('key=',A,' value=',g0)") key, ptr%raw
            type is (real_value)
                write (*, "('key=',A,' value=',g0)") key, ptr%raw
            type is (char_value)
                write (*, "('key=',A,' value=',g0)") key, ptr%raw
            type is (vla_int)
                write (*, "('key=',A,' value is vla_int')") key
            class default
                print *, "other type"
            end select
        end do
    end block

end program main
