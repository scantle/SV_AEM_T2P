program AEM2Texture
  use lognorm
  implicit none
  !-----------------------------------------------------------------------------------------------!
  ! AEM2Texture
  ! Author: Leland Scantlebury, UC Davis leland@scantle.com
  ! March 14, 2023 (Pi Day!)
  !-----------------------------------------------------------------------------------------------!
  integer                    :: ierr, i, iline, nclasses
  real(8), allocatable       :: ln_shape(:), ln_scale(:), ln_loc(:)
  character(30)              :: jnk
  character(60)              :: rho_log_file, tex_out_file, prv_log_file
  character(15), allocatable :: tex_names(:)
  real(8),parameter          :: delta = 1d0, zero = 0.0d0,cutoff=1d-10,NODATA=-999
  
  ! Variables read & written
  integer                    :: id, n, prv_id
  real(8)                    :: x, y, zland, depth, rho
  real(8), allocatable       :: texprob(:)
  character(20)              :: name
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  ! Setup overwrite line
  open(6, carriagecontrol='fortran')
  
  ! Write out program flag
  write(*,'(a)') '_____/\\\\\\\\\_______/\\\\\\\\\______/\\\\\\\\\\\\\\\_        '
  write(*,'(a)') ' ___/\\\\\\\\\\\\\___/\\\///////\\\___\///////\\\/////__       '
  write(*,'(a)') '  __/\\\/////////\\\_\///______\//\\\________\/\\\_______      '
  write(*,'(a)') '   _\/\\\_______\/\\\___________/\\\/_________\/\\\_______     '
  write(*,'(a)') '    _\/\\\\\\\\\\\\\\\________/\\\//___________\/\\\_______    '
  write(*,'(a)') '     _\/\\\/////////\\\_____/\\\//______________\/\\\_______   '
  write(*,'(a)') '      _\/\\\_______\/\\\___/\\\/_________________\/\\\_______  '
  write(*,'(a)') '       _\/\\\_______\/\\\__/\\\\\\\\\\\\\\\_______\/\\\_______ '
  write(*,'(a)') '        _\///________\///__\///////////////________\///________'
  write(*,'(a)') '----------------------------------------------------------------'
  write(*,'(a)') '                     A E M 2 T e x t u r e   v0.02 '
  write(*,'(a)') '-----------------------------------------------------------'
  
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  ! Read input file
  write(*,'(a)') 'Reading AEM2Texture input file'
  open(11, file='AEM2Texture.in', iostat=ierr, status='old')
  read(11,*) jnk, nclasses
  read(11,*) jnk, rho_log_file
  read(11,*) jnk, tex_out_file
  read(11,*) jnk, prv_log_file
  
  ! Allocate texture class arrays
  allocate(tex_names(nclasses), &
           ln_shape (nclasses), &
           ln_scale (nclasses), &
           ln_loc   (nclasses), &
           texprob  (nclasses)  )
  
  read(11,*) jnk   ! Header line
  do i=1, nclasses
    read(11,*) tex_names(i), ln_shape(i), ln_loc(i), ln_scale(i)
  end do
  close(11)
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  ! Loop through log file, writing a new file for T2P while 
  ! converting Rho values to texture probabilities
  write(*,'(2a)') 'Reading AEM Log file:    ', trim(rho_log_file)
  write(*,'(2a)') 'Writing to texture file: ', trim(tex_out_file)
  open(11, file=trim(rho_log_file), iostat=ierr, status='old')
  open(12, file=trim(tex_out_file), iostat=ierr, status='replace')
  
  ! Report header read
  write(6, '(a)') ' - Status: Line 1'
  ! Write alternate header
  write(12,20) 'Line               ', 'ID', 'n', 'X', 'Y', 'Zland', 'Depth', tex_names
  
  ! If present, read/write previous log file as a header
  if (trim(prv_log_file) /= 'NONE') then
    write(6, '("+",a)') ' - Copying previous file...'
    open(13, file=trim(prv_log_file), iostat=ierr, status='old')
    read(13,*)  !header
    read(13,*,iostat=ierr) name, id, n, x, y, zland, depth, texprob  ! first line
    do while (ierr == 0)
      write(12,21) name, id, n, x, y, zland, depth, texprob
      read(13,*,iostat=ierr) name, id, n, x, y, zland, depth, texprob
    end do
    close(13)
    prv_id = id
  else
    prv_id = 0
  end if
  
  ! Read rho header
  read(11,*,iostat=ierr)
  
  ! Read first line
  read(11,*,iostat=ierr) name, id, n, x, y, zland, depth, rho
  
  iline = 2
  do while (ierr == 0)
    ! Report line
    write(6, '("+",a,i7,a)') ' - Status: Line', iline, '      '

    ! If rho is less than lowest approx mean, or greater than highest approx mean, just force to that value
    ! Also watch out for no data values -999
    texprob = zero
    if (rho==NODATA) then
      texprob = NODATA
    else if (rho < minval(ln_scale+ln_loc)) then
      texprob(minloc(ln_scale+ln_loc)) = 1.0d0
    else if (rho > maxval(ln_scale+ln_loc)) then
      texprob(maxloc(ln_scale+ln_loc)) = 1.0d0
    else
      ! Get texture
      do i=1, nclasses
        texprob(i) = lognorm_prob(rho, delta, ln_scale(i), ln_shape(i), ln_loc(i))
        if (texprob(i) < cutoff) texprob(i) = zero
      end do
      if (sum(texprob) <= zero) then
        ! Prevent divide by zero
        write(*,'(a)') 'ERROR - Probabilities <= 0 for all classes'
        stop
      end if
      ! Normalize
      texprob = texprob / sum(texprob)
    end if
    
    ! Write out
    write(12,21) name, prv_id+id, n, x, y, zland, depth, texprob
    
    ! Read in (next) line
    read(11,*,iostat=ierr) name, id, n, x, y, zland, depth, rho
    
    iline = iline + 1
    
  end do

20 format(a20,2(1x,a5),4(1x,a14),100(3x,a12))    
21 format(a20,2(1x,i5),4(1x,f14.5),100(3x,g12.6))
    
  close(11)
  close(12)
  close(6)
  ! Done!
  write(*,'(/,a)') 'EOF reached - Done!'
  !-----------------------------------------------------------------------------------------------!
  
end program AEM2Texture