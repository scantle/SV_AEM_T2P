module mf_fpath
  implicit none
!-----------------------------------------------------------------------------!
! Simple File Path Handling Module - MODFLOW Version
! Author: Leland Scantlebury, 2018
!
!-----------------------------------------------------------------------------!
  integer,   parameter, private     :: maxlen=200  ! Same as MFUSG FNAME length
  character(200)                    :: relpath     ! Used to track relative path to MF files
!-----------------------------------------------------------------------------!

  contains
  
subroutine fpath_strip(path, cfolder, cfile)
!-----------------------------------------------------------------------------!
! Separates a path into folder(s) and file character arrays
! Arguments:
!  - path    (char) filepath
!  - cfolder (char) output, filepath portion of path (NOT divided by folder)
!  - cfile   (char) output, file name portion of path
!-----------------------------------------------------------------------------!
  implicit none
  
  character(*), intent(in)     :: path
  character(*), intent(out)    :: cfolder, cfile
  
  integer                           :: lastslash
  
  lastslash = scan(path, '/\', .true.)
  cfolder = path(1:lastslash)
  cfile = path(lastslash+1:)
  
end subroutine fpath_strip
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine fpath_join(path1, path2, pathcombo)
!-----------------------------------------------------------------------------!
! Joins filepaths path1 & path2 into pathcombo
! A work in progress - many potential situations to handle. Upgrade as needed.
!-----------------------------------------------------------------------------!
  implicit none
  
  character(*), intent(in)  :: path1, path2
  character(*), intent(out) :: pathcombo
  
  character(maxlen)              :: c1, c2
  integer                        :: len1, len2
  
  ! Copy
  c1 = path1
  c2 = path2
  
  ! Set
  len1 = len_trim(c1)
  len2 = len_trim(c2)
  
  ! Checks for path1
  if (len1 > 0) then
    if (c1(len1:len1) /= '/'.and. &
        c1(len1:len1) /= '\') then
      c1 = trim(c1) // '\'
    end if
  end if
        
  ! Checks for path2
  if (c2(1:2)=='./'.or.c2(1:2)=='.\') then
    c2 = c2(3:)
  end if

  pathcombo = adjustl(trim(path1)) // adjustl(trim(path2))
  
end subroutine fpath_join
!-----------------------------------------------------------------------------!

subroutine fname2relative(fname, flen)
!---------------------------------------------------------------------------!
! Used to correct MFUSG file open calls when the name file is located in a
! external (relative) folder
!---------------------------------------------------------------------------!
  implicit none
  
  character(200)                 :: c1
  character(200), intent(inout)  :: fname
  integer,optional,intent(inout) :: flen
  
  ! Copy
  c1 = fname
  
  ! Clear
  fname = " "
  
  ! Call join subroutine (returns dirpath combined with c1 as fname)
  call fpath_join(relpath, c1, fname)
  
  ! Update length argument
  !flen = len_trim(fname)  ! FNAME is padded with JUNK
  if(present(flen)) then
    flen = flen + len_trim(relpath)
  end if
  
end subroutine fname2relative
!-----------------------------------------------------------------------------!

end module mf_fpath
