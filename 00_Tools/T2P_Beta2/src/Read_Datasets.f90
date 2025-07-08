module m_read_datasets
  use m_global ,only: NODATA, log
!  use m_datasets
  use m_T2P, only: t_t2p
  use m_vstring, only: t_vstring, vstring_is
  use m_vstringlist, only: t_vstringlist, vstrlist_search, vstrlist_length,vstrlist_index
  use m_file_io, only: t_file_reader, open_file_reader, item2int, item2char
  use m_error_handler, only: error_handler, warning_handler
  use m_grid, only: t_grid, create_grid_object
  use m_data_transformer, only: t_data_transformer
  implicit none

  type(t_file_reader), pointer :: reader

  contains

!-------------------------------------------------------------------------------------------------!
  subroutine read_datasets(t2p)
    implicit none
    class(t_T2P), intent(inout)   :: t2p

    if (t2p%opt%INTERP_DIM==2) then
      call read_datasets_2D(t2p)
    else if (t2p%opt%INTERP_DIM==3) then
      call read_datasets_3D(t2p)
    else
      ! Shouldn't ever reach here, input checked during m_options::read_options() subroutine
    end if

  end subroutine read_datasets

!-------------------------------------------------------------------------------------------------!

  subroutine read_datasets_2D(t2p)
    implicit none
    class(t_T2P), intent(inout)   :: t2p
    type(t_vstringlist)           :: strings
    character(200)                :: file, name
    character(30),allocatable     :: hsus(:)
    character(256)                :: iomsg
    character(2048)               :: line
    character(20)                 :: tmp,tmp2
    integer                       :: i, j, k, l, ndat, nlines, npoints, nsets, ncats, eof
    integer                       :: idx, idx_offset, iint, prev_idx, prev_iint, fmodel_loc, didx
    integer,allocatable           :: cids(:), all_count(:)
    real                          :: x, y, z, fmodel_z, prev_depth, depth, intv_top, intv_bot, thick, frac_thick
    real    ,allocatable          :: wvalues(:), wv_accumulator(:,:), local_elevs(:), total_thick(:,:)
    real    ,allocatable          :: all_sum(:), all_depth(:)
    type(t_grid),pointer          :: grid
    type(t_data_transformer)      :: data_transformer

    allocate(t2p%datasets(t2p%ndatasets, t2p%fmodel%nlayers))

    ! Setup for HSUs - if a HSU file was given/read, then they are expected in the dataset files
    ! Should we skip if ncat is 1???
    if (t2p%fmodel%hsus%from_file) then
      ncats = t2p%fmodel%nlayers
    else
      ncats = 0
    end if

    ! Loop over files
    do i=1, t2p%ndatafiles
      call item2char(t2p%dataset_files, i, file)
      reader => open_file_reader(file)

      ! Setup data transformer based on information passed to main input file
      ! TODO implement additional parameter(s)
      call data_transformer%set_transform(vstrlist_index(t2p%dataset_trans,i))

      ! Read in header
      call reader%next_item(eof, strings)
      call process_header(t2p, file, strings, nsets, didx, cids)

      ! Allocate for reading
      allocate(wvalues      (nsets))
      allocate(all_count    (nsets))
      allocate(all_sum      (nsets))
      allocate(all_depth    (nsets))
      allocate(total_thick   (nsets,t2p%fmodel%nlayers))
      allocate(wv_accumulator(nsets,t2p%fmodel%nlayers))
      allocate(hsus(ncats))
      if (.not. allocated(local_elevs)) allocate(local_elevs(0:t2p%fmodel%nlayers))
      all_count = 0
      all_sum = 0
      all_depth = 0
      prev_idx = 0
      prev_iint = 0
      prev_depth = 0.0

      ! Get data length (not file length, since we've gone past the header)
      call reader%next_item(eof, strings)
      idx_offset = item2int(strings,2) - 1          ! First well id, must be sequential after
      nlines = reader%get_data_len(rewind_to_last_line=.true.) + 1
      ! Get npoints
      !backspace(reader%unit)  ! EOF
      !backspace(reader%unit)  ! Last line of data
      call reader%next_item(eof, strings)
      npoints = item2int(strings,2) - idx_offset    ! Last - (First - 1) = npoints
      ! Allocate shared grid
      grid => create_grid_object(2, npoints)
      ! Allocate dataset(s) for all layers, set default value
      do k=1, t2p%fmodel%nlayers
        call t2p%datasets(didx,k)%initialize(npoints, nsets)
        t2p%datasets(didx,k)%values = NODATA
        t2p%datasets(didx,k)%grid => grid
        t2p%datasets(didx,k)%id = didx
      end do

      ! For texture classes, if opt%INTERP_ORDER=AFTER we need to store ppzones for these points, too!
      if (didx==1.and.t2p%opt%INTERP_ORDER=="AFTER") then
        call t2p%setup_dataset_ppzones()
      end if

      ! Rewind
      rewind(reader%unit)
      ! Skip header this time...
      call reader%skip(1, eof)

      ! The real work begins
      do j=1, nlines
        write(tmp, '(i)') j+1
        read(reader%unit, "(A)", iostat=reader%ioerr, iomsg=iomsg) line
        read(line, *, iostat=reader%ioerr, iomsg=iomsg) name, idx, iint, x, y, z, depth, wvalues(1:nsets), hsus(1:ncats)
	      call reader%iomsg_handler(iomsg)
        if (didx==1.and.any(wvalues > 1.0)) call error_handler(1,file,"Texture observations cannot be > 1.0, see line: "//adjustl(trim(tmp)))
        all_depth = all_depth + depth

        if (iint==1) then
          idx = idx - idx_offset
          if (idx <= prev_idx) call error_handler(1,file,"Well IDs must be sequential - Check Well "//trim(name)//" on line "//adjustl(trim(tmp)))
          ! Check for duplicate well locations
          do k = 1, idx-1
            if (x==grid%coords(1,k).and.y==grid%coords(2,k)) then
              write(tmp , '(i)') idx+idx_offset
              write(tmp2, '(i)') k+idx_offset
              call error_handler(1,file,"Co-located data point at "//trim(name)//". Loc ID "//trim(adjustl(tmp))//" has same XY as ID "//trim(adjustl(tmp2)))
            end if
          end do
          ! Store previous log values...
          if (j>1) then
            do k=1, t2p%fmodel%nlayers
              do l=1, nsets
                if (total_thick(l,k)>0.0) then
                  t2p%datasets(didx,k)%values(cids(l), prev_idx) = wv_accumulator(l,k) / total_thick(l,k)
                  all_sum(l) = all_sum(l) + wv_accumulator(l,k) / total_thick(l,k)
                  all_count(l) = all_count(l) + 1
                end if
              end do
            end do
          end if
          ! And reset for new log
          wv_accumulator = 0.0
          total_thick = 0.0
          intv_top = z
          thick = depth
          grid%coords(1,idx) = x
          grid%coords(2,idx) = y
          prev_idx = 0
          prev_iint = 0
          prev_depth = 0.0
          ! Get location in model, get elevation at model layers in model
          fmodel_loc = t2p%fmodel%find_in_grid([x,y], t2p%opt%MAX_OUTSIDE_DIST)
          if (fmodel_loc==0) then
            if (t2p%opt%MAX_OUTSIDE_DIST > 0.0) then
              call error_handler(1,file,"Well distance from model greater than MAX_OUTSIDE_DIST: "//name)
            else
              call error_handler(1,file,"Well not found in model domain: "//name)
            end if
          end if
          ! Store location
          do k=1, t2p%fmodel%nlayers
            t2p%datasets(didx,k)%fmodel_loc(idx) = fmodel_loc
          end do
          ! If using, get ppzone in model
          if (t2p%opt%INTERP_ORDER=="AFTER") then
            ! TODO for IWFM this is attempting to match element (fmodel_Loc) to node (ppzones)
            ! Should replace with a fmodel routine get_ppzone_by_loc. For modflow, we'll use the cell, but from iwfm we'll use the nearest node?
            t2p%dataset_ppzones%array(idx,1:) = t2p%dataset_ppzones%array(fmodel_loc,1:)
          end if
          ! Store HSUs for each layer
          do k=1, ncats
            do l=1, nsets
              t2p%datasets(didx,k)%category(idx) = t2p%fmodel%hsus%get_id(hsus(k))
              if (t2p%datasets(didx,k)%category(idx)==0) then
                call error_handler(1,file,"Invalid HSU: "//trim(hsus(k))//" (No matching category in HSU file)")
              end if
            end do
          end do
          call t2p%fmodel%get_elev_at_point(grid%coords(:,idx), local_elevs(:), fmodel_loc)
          fmodel_z = local_elevs(0)
          if (t2p%opt%USE_MODEL_GSE) intv_top = fmodel_z
          prev_idx = idx
        else
          thick = depth - prev_depth
          if (t2p%opt%USE_MODEL_GSE) then
            intv_top = fmodel_z - prev_depth
          else
            intv_top = z - prev_depth
          end if
          ! Some data checking
          if (iint <= prev_iint) call error_handler(1,file,"Well intervals must be sequential - Check Well "//trim(name)//" on line "//adjustl(trim(tmp)))
          if (prev_depth > depth)  call error_handler(1,file,"Bad well depth: Well "//trim(name)//" on line "//adjustl(trim(tmp))//new_line("")//trim(line)//new_line(""))
        end if  ! End of new log handling

        ! Start of accumulator
        ! Transform values
        if (minval(wvalues)>=0) call data_transformer%apply(wvalues)  !TODO Should this be > -999/NODATA?
        if (t2p%opt%USE_MODEL_GSE) then
          intv_bot = fmodel_z - depth
        else
          intv_bot = z - depth
        end if
        ! Accumulate wvalues * interval values for averaging... by layer. It's fun.
        do k=1, t2p%fmodel%nlayers
          if ((intv_top >= local_elevs(k)).and.(intv_bot <= local_elevs(k-1))) then
            ! Find min top/bot values
            frac_thick = min(local_elevs(k-1), intv_top)-max(local_elevs(k), intv_bot)
            do l=1, nsets
              if (wvalues(l)>=0) then     !TODO Should this be > -999/NODATA?
                wv_accumulator(l,k) = wv_accumulator(l,k) + wvalues(l) * frac_thick
                total_thick(l,k) = total_thick(l,k) + frac_thick
              end if
            end do
          end if
        end do
        prev_depth = depth
        prev_iint = iint
      end do

      ! Store final log values
      do k=1, t2p%fmodel%nlayers
        do l=1, nsets
          if (total_thick(l,k)>0.0) then
            t2p%datasets(didx,k)%values(cids(l),prev_idx) = wv_accumulator(l,k) / total_thick(l,k)
            all_sum(l) = all_sum(l) + wv_accumulator(l,k) / total_thick(l,k)
            all_count(l) = all_count(l) + 1
          end if
        end do
      end do

      ! Store means
      do l=1, nsets
        do k=1, t2p%fmodel%nlayers
          t2p%datasets(didx,k)%mean(l) = (all_sum(l) / all_count(l))
        end do
      end do

      ! Cleanup loop variables
      deallocate(wvalues, all_count, all_sum, all_depth, total_thick, wv_accumulator, hsus)
    end do
  end subroutine read_datasets_2D

!-------------------------------------------------------------------------------------------------!

  subroutine read_datasets_3D(t2p)
    implicit none
    class(t_T2P), intent(inout)   :: t2p
    type(t_vstringlist)           :: strings
    character(200)                :: file, name
    character(30),allocatable     :: hsus(:)
    character(256)                :: iomsg
    character(20)                 :: tmp
    integer                       :: i, j, k, l, nlines, npoints, nsets, ncats, eof
    integer                       :: idx, iint, ipoint, prev_idx, prev_iint, fmodel_loc, didx, resized_intervals
    integer                       :: all_count
    integer,allocatable           :: cids(:)
    real                          :: x, y, z, prev_depth, depth, re_depth, thick, fmodel_z
    real    ,allocatable          :: wvalues(:), local_elevs(:), all_thick(:), all_sum(:)
    type(t_grid),pointer          :: grid
    type(t_data_transformer)      :: data_transformer

    allocate(t2p%datasets(t2p%ndatasets, 1)) ! For 3D, we store it all in one layer

    ! Setup for HSUs - if a HSU file was given/read, then they are expected in the dataset files
    ! Should we skip if ncat is 1???
    if (t2p%fmodel%hsus%from_file) then
      ncats = t2p%fmodel%nlayers
    else
      ncats = 0
    end if

    ! Loop over files
    do i=1, t2p%ndatafiles
      call item2char(t2p%dataset_files, i, file)
      reader => open_file_reader(file)

      ! Setup data transformer based on information passed to main input file
      ! TODO implement additional parameter(s)
      call data_transformer%set_transform(vstrlist_index(t2p%dataset_trans,i))

      ! Read in header
      call reader%next_item(eof, strings)
      call process_header(t2p, file, strings, nsets, didx, cids)

      ! Allocate for reading
      allocate(wvalues      (nsets))
      allocate(all_sum      (nsets))
      allocate(all_thick    (nsets))
      allocate(hsus(ncats))
      if (.not. allocated(local_elevs)) allocate(local_elevs(0:t2p%fmodel%nlayers))
      nlines = 0
      all_count = 0
      all_sum = 0
      all_thick = 0
      prev_idx = 0
      prev_iint = 0
      prev_depth = 0.0
      npoints = 0    ! in 3D, npoints =/ nwells. Instead, its the total number of intervals.

      ! Pre-read to get npoints, given opt%MAX_LOG_LENGTH. Logs are to be split evenly into length/MAX_LOG_LENGTH
      do
        read(reader%unit, *, iostat=reader%ioerr, iomsg=iomsg) name, idx, iint, x, y, z, depth
        if (reader%ioerr /= 0) exit   ! done
        nlines = nlines + 1
        if (iint==1) prev_depth = 0.0 ! new log
        ! How many points will this interval become?
        npoints = npoints + ceiling((depth - prev_depth) / t2p%opt%MAX_LOG_LENGTH)
        prev_depth = depth
        !write(*,*) nlines
      end do

      ! Initialize, setup
      grid => create_grid_object(3, npoints)
      call t2p%datasets(didx,1)%initialize(npoints, nsets)
      t2p%datasets(didx,1)%values = NODATA
      t2p%datasets(didx,1)%grid => grid
      t2p%datasets(didx,1)%id = didx

      ! For texture classes, if opt%INTERP_ORDER=AFTER we need to store ppzones for these points, too!
      if (didx==1.and.t2p%opt%INTERP_ORDER=="AFTER") then
        call t2p%setup_dataset_ppzones()
      end if

      ! Rewind
      rewind(reader%unit)
      ! Skip header this time...
      call reader%skip(1, eof)

      ! The real work begins
      ipoint = 1
      do j=1, nlines
        write(tmp, '(i)') j+1
        !write(*,*) tmp // " / ", nlines
        read(reader%unit, *, iostat=reader%ioerr, iomsg=iomsg) name, idx, iint, x, y, z, depth, wvalues(1:nsets), hsus(1:ncats)
	      call reader%iomsg_handler(iomsg)
        if (didx==1.and.any(wvalues > 1.0)) call error_handler(1,file,"Texture observations cannot be > 1.0, see line: "//adjustl(trim(tmp)))
        if (iint <= prev_iint) call error_handler(1,file,"Well intervals must be sequential - Check Well "//trim(name)//" on line "//adjustl(trim(tmp)))
        if (iint==1) then
          prev_depth = 0.0  ! new log
          if (idx <= prev_idx) call error_handler(1,file,"Well IDs must be sequential - Check Well "//trim(name)//" on line "//adjustl(trim(tmp)))
          prev_idx = idx
          ! Get location in model, get top elevation at location
          fmodel_loc = t2p%fmodel%find_in_grid([x,y], t2p%opt%MAX_OUTSIDE_DIST)
          if (fmodel_loc==0) then
            if (t2p%opt%MAX_OUTSIDE_DIST > 0.0) then
              call error_handler(1,file,"Well distance from model greater than MAX_OUTSIDE_DIST: "//name)
            else
              call error_handler(1,file,"Well not found in model domain: "//name)
            end if
          end if
          call t2p%fmodel%get_elev_at_point([x,y], local_elevs(:), fmodel_loc)
          fmodel_z = local_elevs(0)
        end if
        if (prev_depth > depth) call error_handler(1,file,"Bad well depth: Well "//trim(name)//" on line "//adjustl(trim(tmp)))

        thick = (depth - prev_depth)
        resized_intervals = ceiling(thick / t2p%opt%MAX_LOG_LENGTH)

        ! Transform values
        if (minval(wvalues)>=0) then !TODO Should this be > -999/NODATA?
          call data_transformer%apply(wvalues)
        end if

        do k=1, nsets
          if (wvalues(k) > NODATA) then
            all_sum(k) = all_sum(k) + wvalues(k) * thick
            all_thick(k) = all_thick(k) + thick
          end if
        end do

        do k=1, resized_intervals
          grid%coords(1, ipoint) = x
          grid%coords(2, ipoint) = y
          re_depth = prev_depth+(thick/resized_intervals)*(k-1) + ((thick/resized_intervals)/2)
          if (t2p%opt%USE_MODEL_GSE) then
            grid%coords(3, ipoint) = fmodel_z - re_depth
          else
            grid%coords(3, ipoint) = z - re_depth
          end if
          t2p%datasets(didx,1)%values(:, ipoint) = wvalues(1:nsets)
          ! Store location
          t2p%datasets(didx,1)%fmodel_loc(ipoint) = fmodel_loc
          ipoint = ipoint + 1
        end do

        if (depth>0) all_count = all_count + 1
        prev_depth = depth
      end do

      ! Store means
      do j=1, nsets
        if (all_thick(j)>0) then
          t2p%datasets(didx,1)%mean(j) = (all_sum(j) / all_thick(j))
        end if
      end do

      ! Cleanup loop variables
      deallocate(wvalues, all_sum, all_thick, hsus)
    end do
  end subroutine read_datasets_3D

!-------------------------------------------------------------------------------------------------!

  subroutine process_header(t2p, file, header, nsets, dataset_idx, class_ids)
    implicit none
    class(t_T2P), intent(inout)        :: t2p
    character(*),intent(in)            :: file
    type(t_vstringlist),intent(in)     :: header
    integer,intent(out)                :: nsets, dataset_idx
    integer,allocatable                :: temp(:)
    integer,allocatable,intent(out)    :: class_ids(:)
    integer                            :: i, count
    character(30)                      :: badclass
    character(5)                       :: cint

    ! Columns of header should be Location ID n X Y Zland Depth followed by class data, then layer groups
    !                             1        2  3 4 5 6     7
    nsets = vstrlist_length(header) - 7
    if (t2p%has_hsus) nsets = nsets - t2p%fmodel%nlayers
    if (nsets < 1) then
       call error_handler(1,file,"Invalid number of entries in dataset header")
    end if

    allocate(temp(nsets))
    count = 0

    do i=1, nsets
      temp(i) = t2p%get_class_id(vstrlist_index(header,7+i))
      if (temp(i)==0) then
        if (vstring_is(vstrlist_index(header,7+i),'integer')) then
          ! Assumption - an integer indicates this is an (unused) HSU zone == done with classes
          write(cint,'(i5)') count
          call warning_handler("Encountered integer in header for "//trim(file)//" - assuming "//trim(adjustl(cint))//" classes in file")
          exit
        end if
        call item2char(header,7+i,badclass)
        call error_handler(1,file,"Bad class name = " // badclass)
      end if
      count = count + 1
    end do

    ! Move to output variables
    nsets = count
    allocate(class_ids(nsets))
    class_ids(:) = temp(1:nsets)
    deallocate(temp)

    ! Validate datasets either (1) are all the primary datasets or (2) are a secondary dataset
    ! Return index of t2p dataset object to store them in
    if (maxval(class_ids) <= t2p%get_active_texture_classes()) then
      ! is primary...
      if (nsets==t2p%get_active_texture_classes()) then
        dataset_idx = 1
        t2p%class_dataset(class_ids) = dataset_idx
      else
        call error_handler(1,file,"All primary datasets must be in same dataset file")
      end if
    else
      ! Secondary: we adjust all indexes relative to the secondary datasets
      ! secondary_dataset_idx = class_id - n_active_primary_ids + 1
      ! (i.e., the secondary_class_id + 1 for all the primary data being stored in one dataset)
      ! (Each in their own dataset)
      if (nsets==1) then
        dataset_idx = class_ids(1) - t2p%ntexclasses + 1
        t2p%class_dataset(class_ids(1)) = dataset_idx
        class_ids(:) = 1
      else
        call error_handler(1,file,"Each secondary dataset must have their own dataset file")
      end if
    end if
  end subroutine process_header

!-------------------------------------------------------------------------------------------------!

end module m_read_datasets