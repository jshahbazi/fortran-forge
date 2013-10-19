module forge
    use gtk_hl
    use gtk, only: gtk_button_new, gtk_container_add, gtk_label_new, gtk_main, gtk_&
        &main_quit, gtk_menu_item_new, gtk_menu_new, gtk_widget_destroy, gtk_widget_show, &
        & gtk_widget_show_all, gtk_window_new, gtk_init, gtk_events_pending, gtk_main_iteration_do, &
        & gtk_check_menu_item_get_active, gtk_spin_button_get_value, gtk_spin_button_new, &
        & gtk_spin_button_set_value, gtk_radio_button_new, gtk_toggle_button_get_active, GDK_CONTROL_MASK, &
        & CAIRO_LINE_CAP_ROUND, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD, &
        & GTK_FILL, GTK_EXPAND, GTK_SHRINK, gtk_widget_modify_font,gtk_text_view_set_left_margin, &
        & gtk_text_view_set_border_window_size,GTK_TEXT_WINDOW_WIDGET,gtk_misc_set_alignment
        
    use g, only: g_usleep,g_timeout_add

    use cairo, only: cairo_arc, cairo_fill, cairo_fill_preserve, cairo_line_to, &
        & cairo_move_to, cairo_new_path, cairo_paint, cairo_rectangle, &
        & cairo_select_font_face, cairo_set_font_size, cairo_set_line_cap, &
        & cairo_set_line_width, cairo_set_source_rgb, cairo_set_source_rgba, &
        & cairo_show_text, cairo_stroke
    use gtk_draw_hl
    use gdk_events
    use pango
    
    use plplot, PI => PL_PI
    use plplot_extra

  
  
  implicit none
  
!  enum, bind(c)
!    enumerator :: main_window=1,grid_table=2,menu_bar=3,sub_menu=4,text_view=5,text_entry=6
!    enumerator :: label=7,progress_bar=8,button=9
!  end enum

  enum, bind(c)
    enumerator :: widget_expand=1,widget_shrink=2,widget_fill=4
  end enum
  
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
      procedure :: add_non_grid_widget
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
    
      integer(kind=c_int) :: height=250_c_int, width=250_c_int
!  real(kind=c_double), parameter :: pi = 3.14159265358979323846_c_double

    real(plflt) :: xscale, yscale, xoff, yoff
    

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
    subroutine create_window(win,window_name,grid_x_size,grid_y_size,height,width,title,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: window_name
        integer,intent(in) :: grid_x_size, grid_y_size, height, width
        character(len=*),intent(in) :: title
        type(c_ptr) :: event_ptr
        
        win%grid_x_size = grid_x_size
        win%grid_y_size = grid_y_size
        win%title = title//c_null_char
        
        win%main_window = hl_gtk_window_new(win%title,destroy=event_ptr, &
                                          & data_destroy = win%main_window, &
                                          & wsize = (/height, width/), resizable = TRUE)
        win%grid_table=hl_gtk_table_new(nrows=win%grid_x_size,ncols=win%grid_y_size, &
                                        & HOMOGENEOUS=FALSE)  !what's the worst that could happen?
                                       !& row_homogeneous=FALSE, col_homogeneous=FALSE)
        call gtk_container_add(win%main_window, win%grid_table)
    end subroutine create_window
    
    
    subroutine add_widget(self,widget_ptr,widget_name,grid_x,grid_y,xspan,yspan,xopts,yopts,xpad,ypad)
        class(window) :: self
        !type(widget) :: new_widget
        integer :: size_of_current_array,size_of_temp_array,i
        character(len=*) :: widget_name
        type(c_ptr) :: widget_ptr,temp_ptr
        integer,intent(in) :: grid_x,grid_y
        integer,intent(in),optional :: xspan,yspan,xopts,yopts,xpad,ypad
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
            
            if(c_associated(self%get_widget(widget_name))) then
                print *,"Duplicate widget name found:",widget_name
                return
            end if
            
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
        
        temp_ptr=self%get_widget(widget_name) !check for widget
        if(c_associated(temp_ptr))then        !if widget found
            call hl_gtk_table_attach(self%grid_table,widget_ptr,ix=grid_x,iy=grid_y,xspan=xspan, &
                                    & yspan=yspan, xopts=xopts, yopts=yopts)
        else                                  !if widget NOT found
            print *,'Widget not found!'
        end if
    end subroutine add_widget 
    
    
    subroutine add_non_grid_widget(self,widget_ptr,widget_name)
        class(window) :: self
        integer :: size_of_current_array,size_of_temp_array
        character(len=*) :: widget_name
        type(c_ptr) :: widget_ptr
        type(widget), dimension(:), allocatable :: temp_widgets
        
        if(.not. allocated(self%widgets)) then
            !if this is the first widget to be added
            allocate(self%widgets(1))
            self%widgets(1)%widget_ptr = widget_ptr
            !self%widgets(1)%widget_type = widget_type
            self%widgets(1)%widget_name = widget_name
            !print *, 'What am I doing here???'
        else
            size_of_current_array=size(self%widgets)
            
            if(c_associated(self%get_widget(widget_name))) then
                print *,"Duplicate widget name found:",widget_name
                return
            end if
            
            allocate(temp_widgets(size_of_current_array + 1))
            size_of_temp_array=size(temp_widgets)

            !copy existing widgets to temp array
            temp_widgets(1:size_of_current_array)=self%widgets
            !fill in data for new widget
            temp_widgets(size_of_temp_array)%widget_ptr = widget_ptr
            !temp_widgets(size_of_temp_array)%widget_type = widget_type
            temp_widgets(size_of_temp_array)%widget_name = widget_name
            !move allocation of new, full set of widgets back over to the window and get rid of old stuff
            call move_alloc(temp_widgets,self%widgets)
        end if    
    end subroutine add_non_grid_widget 
    
    
    type(c_ptr) function get_widget(self,widget_name)
        class(window) :: self
        character(len=*) :: widget_name
        !character*(len(widget_name)) :: temp
        integer :: i
        
        get_widget= C_NULL_PTR
        
!        temp = trim(widget_name)
!        print *,'trim:',temp,'...'
!        print *,'Looking for:',widget_name,'<'
!        print *,'Listing widget names...'
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
        
        call win%add_widget(menu_bar_ptr,widget_name,grid_x,grid_y,span,yspan=1, &
                            & xopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK), &
                            & yopts= GTK_FILL )
        
    end subroutine create_menu_bar      
    
    
    subroutine create_label(win,widget_name,grid_x,grid_y,span,yspan,text)
        type(window) :: win
        integer,intent(in) :: grid_x,grid_y,span,yspan
        character(len=*),intent(in) :: text, widget_name
        type(c_ptr) :: widget_label
        
        widget_label = gtk_label_new(text//c_null_char)
        
        call gtk_misc_set_alignment(widget_label,0.5,0.5)
        
        
        !call hl_gtk_table_attach(win%grid_table,widget_label,ix=grid_x,iy=grid_y)
        
        call win%add_widget(widget_label,widget_name,grid_x,grid_y,span,yspan,&
                            & xopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK), &
                            & yopts= GTK_FILL )
    end subroutine create_label
    
    
    subroutine create_sub_menu(win,widget_name,item_name)
        type(window) :: win
        type(c_ptr) :: menuptr
        character(len=*) :: widget_name,item_name
        !character(len=60), dimension(:) :: menu_parents
        !character(len=60)::full_name
        integer :: i

!        print *,size(menu_parents)
        !do i = 1, size(menu_parents)
!            print *,widget_name,'-1'
!            print *,trim(widget_name),'-2'
!            menuptr = hl_gtk_menu_submenu_new(win%get_widget(widget_name), trim(menu_parents(i))//c_null_char)
!            full_name =widget_name//menu_parents(i)//c_null_char
!            print *,'Before sub_menu get...'
            menuptr = hl_gtk_menu_submenu_new(win%get_widget(widget_name), item_name//c_null_char)
!            full_name =widget_name//item_name//c_null_char
!            print *,'Fullname:',trim(full_name),'<'
            call win%add_non_grid_widget(menuptr,widget_name//item_name)
            
            
        !end do    
   
!        do i = 1, size(mbuts)
!            write(holder,'("Item: ",I2)') i
!            mbuts(i) = hl_gtk_menu_item_new(smnu, trim(holder)//c_null_char, &
!            & activate=c_funloc(mbut_act), data=c_loc(mclicks(i)), &
!            & accel_key=char(ichar("a")+i-1)//c_null_char, accel_group=accel)
!        end do
    end subroutine create_sub_menu    


    subroutine populate_sub_menu(win,menu_bar_widget,sub_menu_widget,item_name,event_ptr)
        type(window) :: win
        type(c_ptr) :: item_ptr,event_ptr
        character(len=*),intent(in) :: menu_bar_widget,sub_menu_widget,item_name
        !character(len=60)::full_name
        integer :: i
        
        !full_name=trim(menu_bar_widget)//trim(sub_menu_widget)
       ! full_name=menu_bar_widget//sub_menu_widget
!       print *,'Before populate: ',full_name,'-'
        item_ptr = hl_gtk_menu_item_new(win%get_widget(menu_bar_widget//sub_menu_widget), &
                                       &trim(item_name)//c_null_char, activate=event_ptr)
        
    end subroutine populate_sub_menu    
    
    
    subroutine create_combo_box(win,widget_name,combo_options,grid_x,grid_y,span,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        character(len=60), dimension(:) :: combo_options
        integer,intent(in) :: grid_x,grid_y,span
        type(c_ptr) :: combo_box_ptr
        type(c_ptr) :: event_ptr
        type(c_ptr) :: data_ptr

        
        combo_box_ptr = hl_gtk_combo_box_new(changed=event_ptr, data=data_ptr)
        call hl_gtk_table_attach(win%grid_table, combo_box_ptr, ix=grid_x,iy=grid_y,xspan=span, &
                               & xopts= GTK_SHRINK, &
                               & yopts= GTK_SHRINK )
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
        call win%add_widget(progress_bar_ptr,widget_name,grid_x,grid_y,span,&
                           ! & xopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK), 
                            & xopts= GTK_FILL, &
                            & yopts= GTK_SHRINK )
        
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
        call win%add_widget(button_ptr,widget_name,grid_x,grid_y,span,yspan=1, &
                            & xopts= GTK_SHRINK, &
                            & yopts= GTK_SHRINK )
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
        call win%add_widget(text_entry_ptr,widget_name,grid_x,grid_y,span, &
                            & xopts= GTK_FILL, &
                            & yopts= GTK_SHRINK )
    end subroutine create_text_entry    


    subroutine create_text_view(win,widget_name,grid_x,grid_y,xspan,yspan,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: text_view_ptr,view,buffer,scrolled_window,temp_ptr
        type(c_ptr) :: event_ptr
        integer,intent(in) :: grid_x,grid_y,xspan,yspan
        type(c_ptr) :: pango_font_description
        
!        view = gtk_text_view_new()
!        buffer = gtk_text_view_get_buffer (view)
!        call gtk_text_buffer_set_text (buffer, "This is test code.", -1_c_int)
!        scrolled_window = gtk_scrolled_window_new (c_null_ptr, c_null_ptr)
!        call gtk_container_add (scrolled_window, view)
!        call win%add_widget(scrolled_window,widget_name,grid_x,grid_y,xspan,yspan, &
!                         & xopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK), &
!                         & yopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK)  )
!
!        
        
        temp_ptr = hl_gtk_text_view_new(text_view_ptr,editable=TRUE, &      
                                        & changed=event_ptr)!, &
                                        !& ssize=(/size_x,size_y/)) 
                                        
                                        
!        pango_font_description = pango_font_description_from_string("Serif 15"//c_null_char)
!        call gtk_widget_modify_font(temp_ptr,pango_font_description)
        
        call win%add_widget(text_view_ptr,widget_name,grid_x,grid_y,xspan,yspan, &
                         & xopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK), &
                         & yopts= ior(ior(GTK_EXPAND, GTK_FILL), GTK_SHRINK)  )
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
        call win%add_widget(file_chooser_ptr,widget_name,grid_x,grid_y,span,widget_fill)       
    end subroutine create_file_chooser    
    
    
    subroutine create_slider(win,widget_name,grid_x,grid_y,span,length,vertical,min_value,max_value,increment,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: slider_ptr
        type(c_ptr) :: event_ptr
        integer,intent(in) :: grid_x,grid_y,span,length
        real(kind=c_float) :: min_value,max_value,increment
        real(kind=c_double) :: min_value_double,max_value_double,increment_double
        integer(kind=c_int), intent(in), optional :: vertical
        
        !For conversion difficulties
        min_value_double = min_value
        max_value_double = max_value
        increment_double = increment
        
        slider_ptr= hl_gtk_slider_new(min_value_double,max_value_double,increment_double, &
                                     &vertical,value_changed=event_ptr,length=length)
       
        call win%add_widget(slider_ptr,widget_name,grid_x,grid_y,span,widget_fill)
    end subroutine create_slider 
    

    subroutine create_spin_button(win,widget_name,grid_x,grid_y,span,vertical,min_value,max_value,increment,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: spin_ptr
        type(c_ptr) :: event_ptr
        integer,intent(in) :: grid_x,grid_y,span
        real(kind=c_float) :: min_value,max_value,increment
        real(kind=c_double) :: min_value_double,max_value_double,increment_double
        integer(kind=c_int), intent(in), optional :: vertical
        
        !For conversion difficulties
        min_value_double = min_value
        max_value_double = max_value
        increment_double = increment
        
        spin_ptr=hl_gtk_spin_button_new(min_value_double,max_value_double,increment_double, &
                                       & value_changed=event_ptr)
       
        call win%add_widget(spin_ptr,widget_name,grid_x,grid_y,span,widget_fill)
    end subroutine create_spin_button 
    
!    
!    subroutine create_radio_button_group(win,widget_name)
!        type(window) :: win
!        character(len=*),intent(in) :: widget_name
!!        character(len=*),intent(in) :: text
!        type(c_ptr) :: radio_ptr
!        type(c_ptr) :: event_ptr,data_ptr,group_ptr
!!        integer,intent(in) :: grid_x,grid_y,span
!        
!        group_ptr = C_NULL_PTR  !needed
!        
!        call win%add_non_grid_widget(group_ptr,widget_name)
!
!!        radio_ptr = hl_gtk_radio_button_new(group_ptr, text//c_null_char, &
!!                                          & toggled=event_ptr, data=data_ptr)
!
!!        call win%add_widget(radio_ptr,widget_name,grid_x,grid_y,span,widget_fill)
!    end subroutine create_radio_button_group
!    
!    
!    subroutine create_radio_button(win,widget_name,text,grid_x,grid_y,span,event_ptr)
!        type(window) :: win
!        character(len=*),intent(in) :: widget_name
!        type(c_ptr) :: radio_ptr
!        type(c_ptr) :: event_ptr
!        type(c_ptr) :: group_ptr
!        type(c_ptr) :: data_ptr
!        integer,intent(in) :: grid_x,grid_y,span
!        character(len=*) :: text
!        
!        
!        if(.not. c_associated(group_ptr)) then
!            group_ptr = win%get_widget(widget_name)
!        end if    
!        !group_ptr = c_null_ptr
!        radio_ptr = hl_gtk_radio_button_new(group_ptr, text//c_null_char, &
!                                          & toggled=event_ptr, data=data_ptr)
!        
!        call win%add_widget(radio_ptr,widget_name//text,grid_x,grid_y,span,widget_fill)
!        
!    end subroutine create_radio_button    
!    
    subroutine graphics_area_resize(area, data) bind(c)
        type(c_ptr), value :: area, data
        type(gtkallocation), target:: area_alloc

        call gtk_widget_get_allocation(area,c_loc(area_alloc))
!        width = area_alloc%width
!        height = area_alloc%height

        call hl_gtk_drawing_area_resize(area)
    end subroutine graphics_area_resize    


    subroutine create_graphics_area(win,widget_name,grid_x,grid_y,span,yspan,size_x,size_y,event_ptr)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        integer,intent(in) :: grid_x,grid_y,size_x,size_y,span,yspan
        type(c_ptr) :: event_ptr
        type(c_ptr) :: cairo_ptr
    
        cairo_ptr = hl_gtk_drawing_area_new(has_alpha = TRUE,size_allocate=c_funloc(graphics_area_resize) &
                                          &, size = (/size_x,size_y/), &
                                          & button_press_event=event_ptr )
    
        call win%add_widget(cairo_ptr,widget_name,grid_x,grid_y,span,yspan)!, &
                          !  & x_fill_option=widget_fill | widet_expand | widget_shrink, &
                          !  & y_fill_option=widget_fill | widet_expand | widget_shrink)
                          
                      
                          
                          
    end subroutine create_graphics_area


  subroutine x01f95(area)

    type(c_ptr), intent(in) :: area

    type(c_ptr) :: cc

    character(len=80) :: version
    character(len=20) :: geometry
    integer :: digmax
    
    
    

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
         & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170/), &
         & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170/)

    !  Process command-line arguments
    call plparseopts(PL_PARSE_FULL)

    !  Print plplot version
    call plgver(version)
    write (*,'(a,a)') 'PLplot library version: ', trim(version)

    ! Get a cairo context from the drawing area.

    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

!    ! By default the "extcairo" driver does not reset the background
!    ! This is equivalent to the command line option "-drvopt set_background=1"
    call plsetopt("drvopt", "set_background=1")  
!
!    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    call plsetopt("geometry",  geometry)
 
    !  Divide page into 2x2 plots
    call plstar(2,2)

    ! Tell the "extcairo" driver where the context is located. This must be
    ! done AFTER the plstar or plinit call.

    call pl_cmd(PLESC_DEVINIT, cc)

    !  Set up the data
    !  Original case

    xscale = 6._plflt
    yscale = 1._plflt
    xoff = 0._plflt
    yoff = 0._plflt

    !  Do a plot
    call plot1()

    !  Set up the data

    xscale = 1._plflt
    yscale = 0.0014_plflt
    yoff = 0.0185_plflt

    !  Do a plot

    digmax = 5
    call plsyax(digmax,  0)
    call plot1()

    call plot2()
    call plot3()

    !  Don't forget to call PLEND to finish off, and then delete the
    !  cairo context.

    call plend()
    call hl_gtk_drawing_area_cairo_destroy(cc)

  end subroutine x01f95



  !======================================================================
  subroutine plot1()

    real(plflt), dimension(1:60) :: x, y
    real(plflt), dimension(1:6)  :: xs, ys
    real(plflt) :: xmin, xmax, ymin, ymax 
    integer :: i

    do i = 1, 60
       x(i) = xoff + xscale * dble(i)/60.0_plflt
       y(i) = yoff + yscale * x(i)**2
    enddo

    xmin = x(1)
    xmax = x(60)
    ymin = y(1)
    ymax = y(60)

    do i = 1, 6
       xs(i) = x((i-1)*10+4)
       ys(i) = y((i-1)*10+4)
    enddo

    !   Set up the viewport and window using PLENV. The range in X is
    !   0.0 to 6.0, and the range in Y is 0.0 to 30.0. The axes are
    !   scaled separately (just = 0), and we just draw a labelled
    !   box (axis = 0).

    call plcol0(1)
    call plenv( xmin, xmax, ymin, ymax, 0, 0 )
    call plcol0(2)
    call pllab( '(x)', '(y)', '#frPLplot Example 1 - y=x#u2' )

    !   Plot the data points

    call plcol0(4)
    call plpoin( xs, ys, 9 )

    !   Draw the line through the data

    call plcol0(3)
    call plline( x, y )

  end subroutine plot1

  !======================================================================
  subroutine plot2()

    real(plflt), dimension(1:100) :: x, y
    integer :: i

    !
    !   Set up the viewport and window using PLENV. The range in X is
    !   -2.0 to 10.0, and the range in Y is -0.4 to 2.0. The axes are
    !   scaled separately (just = 0), and we draw a box with axes
    !   (axis = 1).

    call plcol0(1)
    call plenv(-2.0_plflt, 10.0_plflt, -0.4_plflt, 1.2_plflt, 0, 1 )
    call plcol0(2)
    call pllab( '(x)', 'sin(x)/x', '#frPLplot Example 1 - Sinc Function' )

    !   Fill up the arrays

    do i = 1, 100
       x(i) = (i-20.0_plflt)/6.0_plflt
       y(i) = 1.0_plflt
       if (x(i) .ne. 0.0_plflt) y(i) = sin(x(i)) / x(i)
    enddo

    !   Draw the line

    call plcol0(3)
    call plwid(2)
    call plline( x, y )
    call plwid(1)

  end subroutine plot2

  !======================================================================
  subroutine plot3()

    !
    !   For the final graph we wish to override the default tick intervals,
    !   and so do not use_ PLENV

    real(plflt), dimension(1:101) :: x, y

    integer i
    call pladv(0)

    !   Use_ standard viewport, and define X range from 0 to 360 degrees,
    !   Y range from -1.2 to 1.2.

    call plvsta()
    call plwind( 0.0_plflt, 360.0_plflt, -1.2_plflt, 1.2_plflt )

    !   Draw a box with ticks spaced 60 degrees apart in X, and 0.2 in Y.

    call plcol0(1)
    call plbox( 'bcnst', 60.0_plflt, 2, 'bcnstv', 0.2_plflt, 2 )

    !   Superimpose a dashed line grid, with 1.5 mm marks and spaces. With
    !   only a single mark and space element, we do not need arrays

    call plstyl( 1, 1500, 1500 )
    call plcol0(2)
    call plbox( 'g', 30.0_plflt, 0, 'g', 0.2_plflt, 0 )
    call plstyl( 0, 0, 0 )

    call plcol0(3)
    call pllab( 'Angle (degrees)', 'sine', '#frPLplot Example 1 - Sine function' )

    do i = 1, 101
       x(i) = 3.6_plflt * (i-1)
       y(i) = sin( x(i) * PI/180.0_plflt )
    enddo

    call plcol0(4)
    call plline( x, y )

  end subroutine plot3
  
  
















    
    subroutine run_on_interval(win,widget_name,event_ptr,interval)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        type(c_ptr) :: event_ptr
        integer(kind=c_int) :: time_id,interval
        
        time_id = g_timeout_add(interval, event_ptr, win%get_widget(widget_name))
    
    end subroutine run_on_interval
    
    

    
!
!  function show_time(area) bind(c)
!    integer(kind=c_int) :: show_time
!    type(c_ptr), value :: area
!
!    integer, dimension(8) :: dat
!    type(c_ptr) :: cr
!    character(len=3) :: sdate
!    
!      integer, dimension(8) :: t0 = 0
!
!    character(len=4), parameter, dimension(12) :: mnames = &
!         & (/'JAN'//c_null_char, 'FEB'//c_null_char, 'MAR'//c_null_char, &
!         &   'APR'//c_null_char, 'MAY'//c_null_char, 'JUN'//c_null_char, &
!         &   'JUL'//c_null_char, 'AUG'//c_null_char, 'SEP'//c_null_char, &
!         &   'OCT'//c_null_char, 'NOV'//c_null_char, 'DEC'//c_null_char /)
!    integer :: i
!    real(kind=c_double) :: r0, r1, x0, x1, y0, y1, th, xc, yc, ycs
!    real(kind=c_double) :: xb, xt, yb, yt, radius, scale_factor
!
!
!    show_time = TRUE
!
!    call date_and_time(values=dat)
!    if (all(dat(5:7) == t0(5:7))) return
!
!    t0 = dat
!
!    cr = hl_gtk_drawing_area_cairo_new(area)
!
!    xc = real(width, c_double) / 2._c_double
!    yc = real(height, c_double) / 2._c_double
!    radius = min(xc, yc)
!    scale_factor = radius/125._c_double
!
!    if (height > width) then
!       xb = 0._c_double
!       xt = real(width, c_double)
!       yt = yc - xc
!       yb = yc + xc
!    else if (height < width) then
!       xb = xc - yc
!       xt = xc + yc
!       yt = 0._c_double
!       yb = real(height, c_double)
!    else
!       xb = 0._c_double
!       xt = real(width, c_double)
!       yt = 0._c_double
!       yb = real(height, c_double)
!    end if
!
!    ! Background
!    call cairo_set_source_rgb(cr, 0.3_c_double, 0.0_c_double, &
!         & 0.0_c_double)
!    call cairo_rectangle(cr, 0._c_double, 0._c_double,&
!         & real(width, c_double), real(height, c_double))
!    call cairo_paint(cr)
!
!    ! Face
!    r0 = radius * 0.85_c_double
!    call cairo_set_source_rgb(cr, 0.3_c_double, 0.3_c_double, 0._c_double)
!    call cairo_new_path(cr)
!    call cairo_move_to(cr, xc+r0, yc)
!    call cairo_arc(cr, xc, yc, r0, 0._c_double, 2.*pi)
!    call cairo_fill(cr)
!
!    ! Sub face
!    r0 =  radius * 0.25_c_double
!    call cairo_set_source_rgb(cr, 0.2_c_double, 0.7_c_double, 0.7_c_double)
!    ycs = yc + 0.375_c_double*radius
!    call cairo_new_path(cr)
!    call cairo_move_to(cr, xc+r0, ycs)
!    call cairo_arc(cr, xc, ycs, r0, 0._c_double, 2.*pi)
!    call cairo_fill(cr)
!
!    ! Clock dials
!    ! Main
!    call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, &
!         & 1._c_double)
!    call cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND)
!    do i = 1, 60
!       if (mod(i,15) == 0) then
!          call cairo_set_line_width(cr, 5._c_double)
!          r0 = radius * 0.75_c_double
!          r1 = radius * 0.9_c_double
!       else if (mod(i,5) == 0) then
!          call cairo_set_line_width(cr, 4._c_double)
!          r0 = radius * 0.8_c_double
!          r1 = radius * 0.9_c_double
!       else
!          call cairo_set_line_width(cr, 2._c_double)
!          r0 = radius * 0.8_c_double
!          r1 = radius * 0.85_c_double
!       end if
!       th = real(i,c_double)*pi/30._c_double
!
!       x0 = sin(th)*r0+xc
!       x1 = sin(th)*r1+xc
!       y0 = cos(th)*r0+yc
!       y1 = cos(th)*r1+yc
!
!       call cairo_move_to(cr, x0, y0)
!       call cairo_line_to(cr, x1, y1)
!       call cairo_stroke(cr)
!    end do
!
!    ! Seconds
!    do i = 1, 60
!       if (mod(i,15) == 0) then
!          call cairo_set_line_width(cr, 2._c_double)
!          r0 = radius * 0.2_c_double
!          r1 = radius * 0.275_c_double
!       else if (mod(i,5) == 0) then
!          call cairo_set_line_width(cr, 1._c_double)
!          r0 = radius * 0.225_c_double
!          r1 = radius * 0.275_c_double
!       else
!          call cairo_set_line_width(cr, 1._c_double)
!          r0 = radius * 0.225_c_double
!          r1 = radius * 0.25_c_double
!       end if
!       th = real(i,c_double)*pi/30._c_double
!
!       x0 = sin(th)*r0+xc
!       x1 = sin(th)*r1+xc
!       y0 = cos(th)*r0+ycs
!       y1 = cos(th)*r1+ycs
!
!       call cairo_move_to(cr, x0, y0)
!       call cairo_line_to(cr, x1, y1)
!       call cairo_stroke(cr)
!    end do
!
!
!    !  Date
!    if (dat(5) >= 12) then
!       call cairo_set_source_rgb(cr, 0._c_double, 0._c_double, 0._c_double)
!    else
!       call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, 1._c_double)
!    end if
!    call cairo_select_font_face(cr, "sans-serif"//c_null_char, &
!         & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
!    x0 = xc - 0.6_c_double*radius
!    call cairo_set_line_width(cr, 1._c_double)
!    call cairo_rectangle(cr, x0-2.*scale_factor, yc+10.*scale_factor, &
!         & 42._c_double*scale_factor,-16._c_double*scale_factor)
!    call cairo_fill_preserve(cr)
!    if (dat(5) < 12) then
!       call cairo_set_source_rgb(cr, 0._c_double, 0._c_double, 0._c_double)
!    else
!       call cairo_set_source_rgb(cr, 1._c_double, 1._c_double, 1._c_double)
!    end if
!    call cairo_stroke(cr)
!    call cairo_set_font_size (cr, 12._c_double*scale_factor)
!    write(sdate,"(I2.2,a1)") dat(3), char(0)
!    call cairo_move_to(cr, x0,yc+6.*scale_factor)
!    call cairo_show_text(cr, sdate)
!    call cairo_set_font_size (cr, 9._c_double*scale_factor)
!    call cairo_show_text(cr, ' '//mnames(dat(2)))
!
!
!    ! Second hand
!    ! Trail
!    th = real(dat(7),c_double) * pi / 30._c_double - pi/2._c_double
!    r0 = radius * 0.24_c_double
!    do i = 1, 15
!       call cairo_set_source_rgba(cr, 1._c_double, 0.1_c_double, &
!            & 1._c_double, 1._c_double-real(i,c_double)/15._c_double)
!       call cairo_new_path(cr)
!       call cairo_move_to(cr, xc, ycs)
!       call cairo_arc(cr, xc, ycs, r0, th-pi/30._c_double, th)
!       call cairo_fill(cr)
!       th = th-pi/30._c_double
!    end do
!
!    ! Hand
!    call cairo_set_source_rgb(cr, .6_c_double, 0.1_c_double, &
!         & .6_c_double)
!    call cairo_set_line_width(cr, 2._c_double)
!
!    th = real(dat(7),c_double) * pi / 30._c_double
!    x1 = r0*sin(th) + xc
!    x0 = xc
!    y1 = -r0*cos(th) + ycs
!    y0 = ycs
!
!    call cairo_move_to(cr, x0, y0)
!    call cairo_line_to(cr, x1, y1)
!    call cairo_stroke(cr)
!
!
!    ! Hour hand
!    call cairo_set_source_rgb(cr, 0.1_c_double, 0.8_c_double, &
!         & 1._c_double)
!    call cairo_set_line_width(cr, 8._c_double)
!    r0 = radius * 0.6_c_double
!    th = (real(mod(dat(5),12),c_double) + &
!         & real(dat(6),c_double)/60._c_double + &
!         & real(dat(7), c_double)/3600._c_double) * pi / 6._c_double
!    x1 = r0*sin(th) + xc
!    x0 = -r0*sin(th)/10._c_double + xc
!    y1 = -r0*cos(th) + yc
!    y0 = r0*cos(th)/10._c_double + yc
!
!    call cairo_move_to(cr, x0, y0)
!    call cairo_line_to(cr, x1, y1)
!    call cairo_stroke(cr)
!
!    ! Minute hand
!    call cairo_set_source_rgba(cr, 1.0_c_double, 1._c_double, &
!         & 0.1_c_double, 0.9_c_double)
!    call cairo_set_line_width(cr, 3._c_double)
!    r0 = min(xc,yc) * 0.85_c_double
!
!    th = (real(dat(6),c_double) + &
!         & real(dat(7),c_double)/60._c_double) * pi / 30._c_double
!    x1 = r0*sin(th) + xc
!    x0 = -r0*sin(th)/10._c_double + xc
!    y1 = -r0*cos(th) + yc
!    y0 = r0*cos(th)/10._c_double + yc
!
!    call cairo_move_to(cr, x0, y0)
!    call cairo_line_to(cr, x1, y1)
!    call cairo_stroke(cr)
!
!    call hl_gtk_drawing_area_cairo_destroy(cr)
!    call gtk_widget_queue_draw(area)
!  end function show_time
!
!    
!



    subroutine create_separator(win,widget_name,grid_x,grid_y,vertical)
        type(window) :: win
        character(len=*),intent(in) :: widget_name
        integer,intent(in) :: grid_x,grid_y
        integer(kind=c_int), intent(in), optional :: vertical
        type(c_ptr) :: separator_ptr
        
        separator_ptr = hl_gtk_separator_new(vertical)
        call win%add_widget(separator_ptr,widget_name,grid_x,grid_y)
    end subroutine create_separator    
    
    
    !call to prevent window from freezing while doing work
    subroutine pending_events()
        integer(c_int) :: boolresult
        do 
            if (gtk_events_pending() == FALSE) exit
            boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
        end do
    end subroutine pending_events
    
end module forge