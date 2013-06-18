module forge
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_label_new, gtk_main, gtk_&
       &main_quit, gtk_menu_item_new, gtk_menu_new, gtk_widget_destroy, gtk_widget_show, &
       & gtk_widget_show_all, gtk_window_new, gtk_init, gtk_events_pending, gtk_main_iteration_do, &
       & gtk_check_menu_item_get_active
  use g, only: g_usleep
  
  implicit none
  
!  enum, bind(c)
!    enumerator :: main_window=1,grid_table=2,menu_bar=3,sub_menu=4,text_view=5,text_entry=6
!    enumerator :: label=7,progress_bar=8,button=9
!  end enum
  
  type widget
      type(c_ptr) :: widget_ptr
      integer :: widget_type
      character(len=60) :: widget_name
      integer :: x_position, y_position
    contains
  end type widget
  
  type window
      integer :: grid_x_size,grid_y_size
      character(len=60) :: title
      type(c_ptr) :: main_window
      type(c_ptr) :: grid_table
      type(widget), dimension(:), allocatable :: widgets
   contains  
      procedure :: add_widget
      procedure :: get_widget
  end type window
  
  
!  
!      type(c_ptr), dimension(10) :: mbuts
!    type(c_ptr) :: mnu2, sm1, sm2
!    type(c_ptr), dimension(4) :: mb1, mb2
!    type(c_ptr) :: rgroup
!
!    type(c_ptr) :: main_window
!                    
!    type(c_ptr) :: win_d, box, box_d, menubar, qbut,lab,lab2, smnu,mba,table,cbox,progress_bar
!   
!    
!    type(c_ptr) :: accel
!    integer(kind=c_int) :: i
!    integer(kind=c_int), dimension(10), target :: mclicks = [ (i-1, i=1,10) ]
!    integer(kind=c_int), target :: mca = -1, gtkmaindo
!    character(len=12) :: holder
!    integer,target :: whichbox = 1
!    type(c_ptr) :: but,view
    
    
    
    

contains

    !---------------------------------------------------
    !Don't modify unless you know what you're doing.
    !Removing any of the GTK+ function calls below
    !will prevent the GUI from working properly
    !Removing the stop call will make any background
    !threads continue even after the GUI is closed.
    subroutine init
        call gtk_init()
    end subroutine init
    !
    subroutine run
        call gtk_main() !loops until user hits quit
        stop !halts all threads and exits the program
    end subroutine run   
    !
    !---------------------------------------------------
  
        !ix is column, iy is row
    

    
    !----------------------------------------------------------
    !Window creation and display procedures
    !
    subroutine create_window(win,window_name,x_size, y_size,title,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: window_name
        integer,intent(in) :: x_size, y_size
        character(len=*),intent(in) :: title
        type(c_ptr) :: event_ptr
        
        win%grid_x_size = x_size
        win%grid_y_size = y_size
        win%title = title//c_null_char
        
        win%main_window = hl_gtk_window_new(win%title,destroy=event_ptr, &
                                          & data_destroy = win%main_window, resizable = TRUE)
        win%grid_table=hl_gtk_table_new(nrows=win%grid_x_size,ncols=win%grid_y_size)
        call gtk_container_add(win%main_window, win%grid_table)
    end subroutine create_window
    
    
    subroutine add_widget(self,widget_ptr,widget_name,grid_x,grid_y,span)
        class(window) :: self
        !type(widget) :: new_widget
        integer :: size_of_current_array,size_of_temp_array
        character(len=60) :: widget_name
        type(c_ptr) :: widget_ptr
        integer,intent(in) :: grid_x,grid_y
        integer,intent(in),optional :: span
        type(widget), dimension(:), allocatable :: temp_widgets
!        type(widget) :: new_widget
        
        if(.not. allocated(self%widgets)) then
            !if this is the first widget to be added
            allocate(self%widgets(1))
            self%widgets(1)%widget_ptr = widget_ptr
            !self%widgets(1)%widget_type = widget_type
            self%widgets(1)%widget_name = widget_name
            self%widgets(1)%x_position = grid_x
            self%widgets(1)%y_position = grid_y
        else
            size_of_current_array=size(self%widgets)
            allocate(temp_widgets(size_of_current_array + 1))
            size_of_temp_array=size(temp_widgets)

            !copy existing widgets to temp array
            temp_widgets(1:size_of_current_array)=self%widgets
            !fill in data for new widget
            temp_widgets(size_of_temp_array)%widget_ptr = widget_ptr
            !temp_widgets(size_of_temp_array)%widget_type = widget_type
            temp_widgets(size_of_temp_array)%widget_name = widget_name
            temp_widgets(size_of_temp_array)%x_position = grid_x
            temp_widgets(size_of_temp_array)%y_position = grid_y
            !move allocation of new, full set of widgets back over to the window and get rid of old stuff
            call move_alloc(temp_widgets,self%widgets)
        end if    
        
        call hl_gtk_table_attach(self%grid_table,self%get_widget(widget_name),ix=grid_x,iy=grid_y,xspan=span)
    end subroutine add_widget 
    
    
    type(c_ptr) function get_widget(self,widget_name)
        class(window) :: self
        character(len=60) :: widget_name
        integer :: i

        do i=1,size(self%widgets)
            if (self%widgets(i)%widget_name == widget_name) then
                get_widget = self%widgets(i)%widget_ptr
                return
            end if
        end do    
    end function get_widget    
    
    
    subroutine show_window(win)
        type(window) :: win
        
        call gtk_widget_show_all(win%main_window)
    end subroutine show_window    
    !
    !----------------------------------------------------------
    
    
    !----------------------------------------------------------
    !Widgets
    !
    subroutine create_menu_bar(win,widget_name,grid_x,grid_y,span) 
        type(window) :: win
        integer,intent(in) :: grid_x,grid_y,span
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: menu_bar_ptr
        
        menu_bar_ptr = hl_gtk_menu_new(GTK_PACK_DIRECTION_LTR)
        !call hl_gtk_table_attach(win%grid_table, win%menu_bar,ix=grid_x,iy=grid_y,xspan=span)
        
        call win%add_widget(menu_bar_ptr,widget_name,grid_x,grid_y,span)
    end subroutine create_menu_bar      
    
    
    subroutine create_label(win,widget_name,grid_x,grid_y,span,text)
        type(window) :: win
        integer,intent(in) :: grid_x,grid_y,span
        character(len=*),intent(in) :: text, widget_name
        type(c_ptr) :: widget_label
        
        widget_label = gtk_label_new(text//c_null_char)
        !call hl_gtk_table_attach(win%grid_table,widget_label,ix=grid_x,iy=grid_y)
        
        call win%add_widget(widget_label,widget_name,grid_x,grid_y,span)
    end subroutine create_label
    
    

    subroutine create_sub_menu(win,widget_name)
        type(window) :: win
        type(c_ptr) :: winptr,menuptr_file,menuptr_edit,menuptr_quit
        character(len=*),intent(in) :: widget_name
        
        winptr = win%main_window
        
        menuptr_file = hl_gtk_menu_submenu_new(win%get_widget(widget_name), "File"//c_null_char)

!        do i = 1, size(mbuts)
!            write(holder,'("Item: ",I2)') i
!            mbuts(i) = hl_gtk_menu_item_new(smnu, trim(holder)//c_null_char, &
!            & activate=c_funloc(mbut_act), data=c_loc(mclicks(i)), &
!            & accel_key=char(ichar("a")+i-1)//c_null_char, accel_group=accel)
!        end do
        menuptr_edit =  hl_gtk_menu_item_new(win%get_widget(widget_name), "Edit"//c_null_char)!, activate=c_funloc(mbut_act), data=c_loc(mca))
        menuptr_quit = hl_gtk_menu_item_new(win%get_widget(widget_name), "Quit"//c_null_char)!, activate=c_funloc(event_destroy), data=winptr)
    end subroutine create_sub_menu    

    
    !add text array to pass in options maybe???
    subroutine create_combo_box(win,widget_name,combo_options,grid_x,grid_y,span,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        character(len=60), dimension(:) :: combo_options
        integer,intent(in) :: grid_x,grid_y,span
        type(c_ptr) :: combo_box_ptr
        type(c_ptr) :: event_ptr
        type(c_ptr) :: data_ptr

        
        combo_box_ptr = hl_gtk_combo_box_new(changed=event_ptr, data=data_ptr)
        call hl_gtk_table_attach(win%grid_table, combo_box_ptr, ix=3,iy=3)
        call hl_gtk_combo_box_add_text(combo_box_ptr,combo_options(1)//c_null_char,at_start=TRUE)
        call hl_gtk_combo_box_add_text(combo_box_ptr,combo_options(2)//c_null_char,index=1)
!        call hl_gtk_combo_box_add_text(combo_box_ptr,"Option 3"//c_null_char,index=2)
!        call hl_gtk_combo_box_add_text(combo_box_ptr,"Option 4"//c_null_char,index=3)
!        call hl_gtk_combo_box_add_text(combo_box_ptr,"Option 5"//c_null_char,index=4)
    end subroutine create_combo_box    


    subroutine create_progress_bar(win,widget_name,grid_x,grid_y,span,text)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        integer,intent(in) :: grid_x,grid_y,span
        type(c_ptr) :: progress_bar_ptr
        character(len=*),intent(in),optional :: text
        
        progress_bar_ptr = hl_gtk_progress_bar_new()
        !call hl_gtk_table_attach(win%grid_table, progress_bar_ptr, ix=1,iy=3)
        call win%add_widget(progress_bar_ptr,widget_name,grid_x,grid_y,span)
        
        call hl_gtk_progress_bar_set_f(progress_bar_ptr, text=text//c_null_char)  !pulsed
        !call hl_gtk_progress_bar_set_f(progress_bar_ptr, val=0.50_c_double, string=TRUE)
    end subroutine create_progress_bar
    
    
    subroutine set_progress_bar(win,widget_name,progress_bar_value)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        real :: progress_bar_value
        
        call pending_events() !do this so you can use a progress while doing stuff
        call hl_gtk_progress_bar_set(win%get_widget(widget_name), val=real(progress_bar_value,kind=8), string=TRUE)
    end subroutine set_progress_bar    


    subroutine create_button(win,widget_name,grid_x,grid_y,span,text,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: button_ptr
        type(c_ptr) :: event_ptr
        integer,intent(in) :: grid_x,grid_y,span
        character(len=*),intent(in) :: text
        
        button_ptr=hl_gtk_button_new(text//c_null_char, clicked=event_ptr)
        call win%add_widget(button_ptr,widget_name,grid_x,grid_y,span)
    end subroutine create_button    
 
 
    subroutine create_text_entry(win,widget_name,grid_x,grid_y,span,text,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: text_entry_ptr
        type(c_ptr) :: event_ptr
        integer,intent(in) :: grid_x,grid_y,span
        character(len=*),intent(in) :: text
       
        text_entry_ptr = hl_gtk_entry_new(60_c_int, editable=TRUE, &
                                    & tooltip=text//c_null_char, changed=event_ptr)
        call win%add_widget(text_entry_ptr,widget_name,grid_x,grid_y,span)
    end subroutine create_text_entry    


    subroutine create_text_view(win,widget_name,grid_x,grid_y,size_x,size_y,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: text_view_ptr,temp_ptr
        type(c_ptr) :: event_ptr
        integer,intent(in) :: grid_x,grid_y,size_x,size_y
        
        temp_ptr = hl_gtk_text_view_new(text_view_ptr,editable=TRUE, &      
                                        & changed=event_ptr, &
                                        & ssize=(/size_x,size_y/)) 
       call win%add_widget(text_view_ptr,widget_name,grid_x,grid_y)
    end subroutine create_text_view    


    subroutine create_file_chooser(win,widget_name,grid_x,grid_y,span,text,filters,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: file_chooser_ptr
        type(c_ptr) :: event_ptr
        character(len=*),intent(in) :: text
        integer,intent(in) :: grid_x,grid_y,span
        character(len=*), dimension(:),intent(in)  :: filters
        
        file_chooser_ptr = hl_gtk_file_chooser_button_new(title=text//c_null_char, &
                                                        & filter=filters, file_set=event_ptr)
       call win%add_widget(file_chooser_ptr,widget_name,grid_x,grid_y,span)       
    end subroutine create_file_chooser    
    
    

    !call to prevent window from freezing while doing work
    subroutine pending_events()
        integer(c_int) :: boolresult
        do 
            if (gtk_events_pending() == FALSE) exit
            boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
        end do
    end subroutine pending_events
    
end module forge