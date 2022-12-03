program main
    use machina_vla
    use machina_basic, only: i4, f8
    implicit none

    call test_vla_int()
    call test_vla_bool()
    call test_vla_real()

contains

    subroutine test_vla_int()
        type(vla_int) :: v
        integer :: i

        write (*, "('=== test_vla_bool ===')")

        call v%push_back([1, 2, 3, 4, 5, 6])

        i = 0
        block
            type(vla_int_iterator) :: it
            it = v%iterator()

            do while (it%has_next())
                it%get_next() = i
                i = i + 1
            end do
        end block

        block
            type(vla_int_const_iterator) :: it
            it = v%const_iterator()

            do while (it%has_next())
                print *, it%get_next()
            end do
        end block

        write (*, "(A)") repeat('=', 20)

    end subroutine test_vla_int

    subroutine test_vla_bool()
        type(vla_bool) :: v
        logical :: b

        write (*, "('=== test_vla_bool ===')")

        call v%push_back(.false.)
        call v%push_back(.true.)
        call v%push_back([.false., .false.])

        block
            type(vla_bool_iterator) :: it
            it = v%iterator(reverse=.true.)

            do while (it%has_next())
                print *, it%get_next()
            end do
        end block

        call v%pop(b)
        call v%shift(b)

        block
            type(vla_bool_const_iterator) :: it
            it = v%const_iterator()

            do while (it%has_next())
                print *, it%get_next()
            end do
        end block

        write (*, "(A)") repeat('=', 20)

    end subroutine test_vla_bool

    subroutine test_vla_real()
        type(vla_real) :: v
        real(kind=f8) :: r
        integer :: i

        write (*, "('=== test_vla_real ===')")

        call v%push_back([(cos(real(i, f8)), i=1, 20)])
        call v%push_back(-1._f8)
        write (*, "('Size of vla is ',g0)") size(v)

        do i = 1, 14
            call v%shift(r)
        end do

        block
            type(vla_real_const_iterator) :: it
            it = v%const_iterator()

            do while (it%has_next())
                print *, it%get_next()
            end do
        end block

        write (*, "(A)") repeat('=', 20)

    end subroutine test_vla_real

end program main
