!ForGE v0.4.0
!Fortran GUI Environment
!John N. Shahbazian
!
!Uses the gtk-fortran gtk+ Fortran Interface library, GtkFortran High Level API
!Released under the GNU Public License (GPL), v3 or later
!See http://www.gnu.org/licenses/

#define event(x) c_funloc(x)

module event_functions
    use iso_c_binding
    use forge
    
    contains
    
    
    subroutine test_draw(area) bind(c)
        type(c_ptr), value :: area
        type(c_ptr) :: drawing_area
        real(kind=c_double) :: r,g,b
        real(kind=c_double) :: xc,yc,radius,scale_factor
        type(gtkallocation), target:: alloc
        integer(kind=c_int) :: height, width
        
        drawing_area = hl_gtk_drawing_area_cairo_new(area)
        r=0.3
        g=0.0
        b=0.0
        
        call gtk_widget_get_allocation(area,c_loc(alloc))
        width = alloc%width
        height = alloc%height
        xc = real(width, c_double) / 2._c_double
        yc = real(height, c_double) / 2._c_double
        radius = min(xc, yc)
        scale_factor = radius/125._c_double
    
        call cairo_set_source_rgb(drawing_area, 1.0_c_double, 1.0_c_double, 1.0_c_double)
        call cairo_rectangle(drawing_area, 0._c_double, 0._c_double,real(width, c_double), real(height, c_double))
        call cairo_paint(drawing_area)
        call cairo_select_font_face(drawing_area, "sans-serif"//c_null_char,CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
        call cairo_set_font_size (drawing_area, 9._c_double*scale_factor)
        
        call cairo_set_source_rgb(drawing_area, 0.0_c_double, 0.0_c_double, 0._c_double)
        call cairo_set_line_width(drawing_area, 5._c_double)
        call cairo_move_to(drawing_area, 5._c_double,yc+6.*scale_factor)
        call cairo_line_to(drawing_area, 100._c_double,yc+7.*scale_factor)
       call cairo_stroke(drawing_area)
      ! call cairo_fill(drawing_area)
      !  call cairo_show_text(drawing_area,'Drawing area')
    
        call hl_gtk_drawing_area_cairo_destroy(drawing_area)
        call gtk_widget_queue_draw(area)
    end subroutine
        
        
    
    subroutine event_destroy(widget_ptr,window_ptr) bind(c)
        type(c_ptr), value :: widget_ptr, window_ptr
        print *, "Exit called"

        !    if (c_associated(window_ptr)) then
        !        call gtk_widget_destroy(window_ptr)
        !    end if
        call gtk_main_quit ()
    end subroutine event_destroy


    subroutine event_button(widget_ptr,data_ptr) bind(c)
        type(c_ptr), value :: widget_ptr,data_ptr
         
        print *, "Button pushed"  

    end subroutine event_button


    subroutine event_text_entry(widget_ptr,data_ptr) bind(c)
        type(c_ptr), value :: widget_ptr,data_ptr
        character(len=60) :: text
        integer(kind=c_int16_t) :: ntext
        integer :: status

        ntext = gtk_entry_get_text_length(widget_ptr)
        print *, ntext
        if (ntext > 5) then
            call hl_gtk_entry_get_text(widget_ptr,text,status)
            print *,text
        end if    
    end subroutine event_text_entry

 
    subroutine event_text_view(widget_ptr,data_ptr) bind(c)
        type(c_ptr), value :: widget_ptr,data_ptr

        character(len=200), dimension(:), allocatable:: textstuff

        call hl_gtk_text_view_get_text(c_null_ptr,textstuff,buffer=widget_ptr)
        print *, textstuff
        deallocate(textstuff)
    end subroutine event_text_view   
    

    subroutine event_file_chooser_open(widget, gdata) bind(c)
!        type(c_ptr), value :: widget, gdata
!
!        type(c_ptr) :: c_string
!        character(len=200) :: inln
!        integer :: ios
!        integer :: idxs
!
!        c_string = gtk_file_chooser_get_filename(widget)
!        call convert_c_string(c_string, filename)
!        call g_free(c_string)
!
!        open(37, file=filename, action='read')
!        call hl_gtk_text_view_delete(tedit)
!        do
!            read(37,"(A)",iostat=ios) inln
!            if (ios /= 0) exit
!            call hl_gtk_text_view_insert(tedit, (/ trim(inln)//c_new_line /))
!        end do
!        close(37)
!        idxs = index(filename, '/', .true.)+1
!        call gtk_window_set_title(window, trim(filename(idxs:))//c_null_char)
!
!        ! We manually reset the changed flag as the text box signal handler sets it.
!
!        file_is_changed = .FALSE.
!        call gtk_widget_set_sensitive(sabut, TRUE)
!        call gtk_widget_set_sensitive(sbut, FALSE)
    end subroutine event_file_chooser_open
      
    
    
    
    
!    
!      subroutine mbut_act(widget, gdata)  bind(c)
!    type(c_ptr), value :: widget, gdata
!    integer(kind=c_int), pointer :: fdata
!
!    print *, "Menu 1"
!    if (c_associated(gdata)) then
!        
!       call c_f_pointer(gdata, fdata)
!       if (fdata < 0) then
!          print *, "Chose: Extra"
!       else
!          print *, "Chose:",fdata
!       end if
!    end if
!
!  end subroutine mbut_act


  subroutine menu_open(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    
    print *, "Menu Open"
  end subroutine menu_open

  subroutine menu_close(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    
    print *, "Menu Close"
  end subroutine menu_close
  
  subroutine menu_save(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    
    print *, "Menu Save"
  end subroutine menu_save
  
  subroutine menu_about(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    
    print *, "Best GUI ever."
  end subroutine menu_about


  subroutine sm2_act(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int), pointer :: fdata
    integer(kind=c_int) :: istat

    print *, "Menu 2 (submenu)"
    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       istat = gtk_check_menu_item_get_active(widget)
       if (istat == TRUE) then
          print *, "Sub-Selected:",fdata
       else
          print *, "Sub-Deselected:",fdata
       end if
    end if
  end subroutine sm2_act


  subroutine c_change(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer, pointer :: index
    integer(kind=c_int) :: isel, nrow
    character(len=40) :: value

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, index)
       print "('ComboBox:',I2)", index
    end if

    isel = hl_gtk_combo_box_get_active(widget, ftext=value)
    nrow = hl_gtk_combo_box_n_entries(widget)

    print "('Choice:',I2,' of ',i2,' Text:',a)", isel, nrow, trim(value)
  end subroutine c_change



    !--------------------------------
    !dialog handlers
!  subroutine msg_alert(widget, gdata) bind(c)
!    type(c_ptr), value :: widget, gdata
!
!    integer(kind=c_int) :: resp
!
!    character(len=40), dimension(5) :: msg
!
!    msg(1) = "ALERT"
!    msg(2) = ""
!    msg(3) = "You have pressed an alert button"
!    msg(4) = ""
!    msg(5) = "You know that's dangerous"
!
!    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_OK,"ALERT"//c_null_char,type=GTK_MESSAGE_WARNING, parent=main_window)
!  end subroutine msg_alert
!
!
!  subroutine msg_quit(widget, gdata) bind(c)
!    type(c_ptr), value :: widget, gdata
!
!    integer(kind=c_int) :: resp
!
!    character(len=40), dimension(3) :: msg
!
!    msg(1) ="QUIT?"
!    msg(2) = ""
!    msg(3) = "Do you really want to quit?"
!
!    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
!         & "QUIT"//c_null_char, parent=main_window)
!    if (resp == GTK_RESPONSE_YES) call gtk_main_quit()
!
!  end subroutine msg_quit
!
!
!  subroutine msg_about(widget, gdata) bind(c)
!    type(c_ptr), value :: widget, gdata
!
!    call hl_gtk_about_dialog_gtk_fortran(main_window)
!
!  end subroutine msg_about
  !---------------------------------------------
    
end module event_functions  




program test
    use event_functions
    use forge
    
        
    implicit none
    
    !-----------------------------------------------
    !Declare window objects
    !
    type(window) :: guiwindow
    !
    !------------------------------------------------
    character(len=60), dimension(2) :: combo_options


    call init()
    call setup()
    call run()
contains

    subroutine setup
        !If you want to create multiple windows in here, you can.  Just declare another
        !type(window) above and the below, call procedures for it.  The minimum required procedures
        !are set_parameters, create_window, and show_window
         
        !ix is column, iy is row
         
        call create_window(guiwindow,"window1",8,8,800,400,"Title",event(event_destroy))
        
        
        call create_menu_bar(guiwindow,"menubar1",0,0,8)
        call create_sub_menu(guiwindow,"menubar1","File")
        call populate_sub_menu(guiwindow,"menubar1","File","Open",event(menu_open))
        call populate_sub_menu(guiwindow,"menubar1","File","Close",event(menu_close))
        call populate_sub_menu(guiwindow,"menubar1","File","Save",event(menu_save))
        call populate_sub_menu(guiwindow,"menubar1","File","Quit",event(event_destroy))
        call create_sub_menu(guiwindow,"menubar1","Edit")
        call populate_sub_menu(guiwindow,"menubar1","Edit","Cut",event(menu_open))
        call populate_sub_menu(guiwindow,"menubar1","Edit","Copy",event(menu_open))
        call populate_sub_menu(guiwindow,"menubar1","Edit","Paste",event(menu_open))
        call create_sub_menu(guiwindow,"menubar1","About")
        call populate_sub_menu(guiwindow,"menubar1","About","About ForGE",event(menu_about))
        
        
        call create_label(guiwindow,"label1",0,1,4,1,"Menu Example")
        !call create_label(guiwindow,"label2",1,2,1,"Menu Example")
        !call create_label(guiwindow,"label2",2,3,1,"Second Line")
        !call create_button(guiwindow,"button1",0,1,1,"Button",event(event_button))
        call create_button(guiwindow,"button2",0,2,4,"Button v2.0",event(event_button))
        !call create_slider(guiwindow,"slider1",0,1,1,200,false,0.0,10.0,0.2,event(event_button))
!        call create_spin_button(guiwindow,"spin1",0,2,1,false,0.0,10.0,0.2,event(event_button))
        !call create_separator(guiwindow,"sep1",0,2,TRUE)
!        call create_text_entry(guiwindow,"textentry1",1,2,1,"Enter text",event(event_text_entry))
        call create_label(guiwindow,"label2",0,3,1,1,"Text View:")
        call create_text_view(guiwindow,"textview1",1,3,3,5,event(event_text_view))
        !call create_file_chooser(guiwindow,"chooser1",0,3,2,"Open File",(/"*.txt","*.f90"/),event(event_file_chooser_open))
!        call create_label(guiwindow,2,3,"Menu Ex2")
!        call create_radio_button_group(guiwindow,"radio1")
!        call create_radio_button(guiwindow,"radio1","wat",0,2,1,event(event_button))
!        call create_radio_button(guiwindow,"radio1","wut",0,3,1,event(event_button))
        
        !combo_options(1) = "Option 1"
        !combo_options(2) = "Option 2"
        
        !call create_combo_box(guiwindow,"combo1",combo_options,2,2,1,event(c_change))
!        call create_progress_bar(guiwindow,"progress1",0,3,2)
!        call set_progress_bar(guiwindow,"progress1",0.5)

        call create_graphics_area(guiwindow,"cairo1",4,1,4,7,300,300)
        call run_on_interval(guiwindow,"cairo1",event(test_draw),300)
    
  
        call show_window(guiwindow)

        !dialog window        
!        call set_parameters(dialog,3,1,"Dialog Test")
!        call create_window(dialog)
!        call create_button(dialog,1,1,"Ok")
!        call create_button(dialog,2,1,"Retry")
!        call create_button(dialog,3,1,"Cancel")
!        !call create_dialog_box(dialog)
!        call show_window(dialog)
        
    end subroutine setup    

end program test

  
!added sub-menus, separator, sliders, spin buttons, cairo
!fixed: duplicate widget name

!todo: add radio (do later)







