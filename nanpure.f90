program nanpure
  implicit none 
  integer :: i=0,j=0,k,l, a(9,9)=0
  logical :: fg=.true. , a0(9,9)=.false.

  ! 入力
  do i = 1, 9
    do j = 1, 9
      read *, a(j,i)
      if(a(j,i) == 0)a0(j,i) = .true.
    end do
  end do
  i=1
  j=1
  
  do while (i<9)
    c:do while(j<9)
      ! 初期値のないマスに1から9まで代入していく
      if(a0(j,i) )then
        d:do while(a(j,i) < 9)
          a(j,i)=a(j,i)+1
          

          ! 衝突調査
          e:do k = 1,9
            if(i /= k .and. a(j,k) == a(j,i)) then
              fg=.false.
              exit e
            end if
            if(j /= k .and. a(k,i) == a(j,i)) then
              fg=.false.
              exit e
            end if
          end do e
          f:do k = 1,9
            do l = 1,9
              if ((i-1)/3 == (k-1)/3 .and. (j-1)/3 == (l-1)/3 .and. (i /= k .or. j /= l) .and. a(j,i) == a(l,k)) then
                fg = .false.
                exit f
              end if
            end do
          end do f
          

          ! 衝突した時とそうでない時の分岐  
          if(fg) then
              exit d
          else if(a(j,i)==9)then
            if(j==1) then
              if(i==1) then
                  stop "error"
              else
                  i=i-2
                  j=8
                  exit c
              end if
            else 
              j=j-2
            end if
            exit d
          end if
          
          
        end do d
      end if
      j=j+1
    end do c
  i=i+1
  end do

  !プリント
  do i=1,9
    do j=1,9
      if(j/=9)then 
        print '(I4$)',a(j,i)
      else 
        print  '(I4)',a(j,i)
      end if
    end do
  end do

end program nanpure
    
    
