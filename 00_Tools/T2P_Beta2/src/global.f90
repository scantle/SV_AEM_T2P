module m_global
  use m_file_io, only: t_file_writer
  
  type(t_file_writer), pointer     :: log   ! Runtime Log
  real                             :: NODATA = -999
  
end module m_global