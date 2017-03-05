extern crate gtk;
extern crate gdk;
extern crate gdk_sys;
extern crate gdk_pixbuf;
extern crate cairo;
extern crate screenshot;
extern crate image;
extern crate epoxy;
extern crate gl;
extern crate shared_library;

use gtk::prelude::*;
use screenshot::get_screenshot;
use self::gl::types::*;
use std::cell::{RefCell, Cell};
use std::f64::consts::PI;
use std::ffi::CStr;
use std::mem;
use std::ptr;
use std::rc::Rc;

#[macro_export]
macro_rules! clone {
    (@param _) => ( _ );
    (@param $x:ident) => ( $x );
    ($($n:ident),+ => move || $body:expr) => (
        {
            $( let $n = $n.clone(); )+
            move || $body
        }
    );
    ($($n:ident),+ => move |$($p:tt),+| $body:expr) => (
        {
            $( let $n = $n.clone(); )+
            move |$(clone!(@param $p),)+| $body
        }
    );
}

#[derive(Default, Copy, Clone)]
struct Circle {
    position: (f64, f64),
    radius: f64
}

impl Circle {
    fn contains(&self, (px, py): (f64, f64)) -> bool {
        let (cx, cy) = self.position;

        let dx = (px - cx).abs();
        let dy = (py - cy).abs();

        let distance = (dx.powi(2) + dy.powi(2)).sqrt();

        distance <= self.radius
    }
}

/// Used to keep track of the user's input
const PATTERN_MAXLENGTH: usize = 9;
#[derive(Default)]
struct PatternData {
    current_input: [u8; PATTERN_MAXLENGTH],
    next_digit: usize,
    drawing: bool,
    draw_position: (f64, f64),
    digit_areas: [Circle; 9]
}

impl PatternData {
    fn reset_input(&mut self) {
        self.current_input = [0; 9];
        self.next_digit = 0;
    }

    fn input_contains(&self, digit: u8) -> bool {
        for &n in &self.current_input {
            if digit == n { return true }
        }

        false
    }

    fn can_input(&self, digit: u8) -> bool {
        if self.next_digit >= PATTERN_MAXLENGTH { return false }

        !self.input_contains(digit)
    }

    fn input_direct(&mut self, digit: u8) {
        self.current_input[self.next_digit] = digit;
        self.next_digit += 1;
    }

    fn input_if_possible(&mut self, digit: u8) {
        if self.can_input(digit) { self.input_direct(digit) }
    }

    fn input(&mut self, digit: u8) {
        // First input
        if self.next_digit == 0 {
            self.current_input[0] = digit;
            self.next_digit += 1;
            return;
        }

        // Last input
        if self.next_digit == PATTERN_MAXLENGTH - 1 {
            self.current_input[self.next_digit] = digit;
            self.next_digit = PATTERN_MAXLENGTH;
            return;
        }

        // Also input digits "on the way" to the selected digit
        let last_digit = self.current_input[self.next_digit - 1];
        match (last_digit, digit) {
            (1, 3) | (3, 1) => { self.input_if_possible(2); self.input_direct(digit) },
            (4, 6) | (6, 4) => { self.input_if_possible(5); self.input_direct(digit) },
            (7, 9) | (9, 7) => { self.input_if_possible(8); self.input_direct(digit) },
            (1, 7) | (7, 1) => { self.input_if_possible(4); self.input_direct(digit) },
            (2, 8) | (8, 2) => { self.input_if_possible(5); self.input_direct(digit) },
            (3, 9) | (9, 3) => { self.input_if_possible(6); self.input_direct(digit) },
            (1, 9) | (9, 1) => { self.input_if_possible(5); self.input_direct(digit) },
            (3, 7) | (7, 3) => { self.input_if_possible(5); self.input_direct(digit) },
            _ => self.input_direct(digit)
        }
    }
}

fn compile_shader(src: &str, ty: GLenum) -> Result<GLuint, String> {
    unsafe {
        let shader = gl::CreateShader(ty);
        // Attempt to compile the shader
        let psrc = src.as_ptr() as *const GLchar;
        let len = src.len() as GLint;
        gl::ShaderSource(shader, 1, &psrc, &len);
        gl::CompileShader(shader);

        // Get the compile status
        let mut status = epoxy::FALSE as GLint;
        gl::GetShaderiv(shader, epoxy::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != (epoxy::TRUE as GLint) {
            let mut len = 0;
            gl::GetShaderiv(shader, epoxy::INFO_LOG_LENGTH, &mut len);
            let mut buf = vec![0i8; len as usize];
            gl::GetShaderInfoLog(shader, len, ptr::null_mut(), buf.as_mut_ptr() as *mut GLchar);
            return Err(CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned())
        }

        Ok(shader)
    }
}

fn link_program(vs: GLuint, fs: GLuint) -> Result<GLuint, String> {
    unsafe {
        let program = gl::CreateProgram();
        gl::AttachShader(program, vs);
        gl::AttachShader(program, fs);
        gl::LinkProgram(program);

        // Get the link status
        let mut status = epoxy::FALSE as GLint;
        gl::GetProgramiv(program, epoxy::LINK_STATUS, &mut status);

        // Fail on error
        if status != (epoxy::TRUE as GLint) {
            let mut len: GLint = 0;
            gl::GetProgramiv(program, epoxy::INFO_LOG_LENGTH, &mut len);
            let mut buf = vec![0i8; len as usize];
            gl::GetProgramInfoLog(program, len, ptr::null_mut(), buf.as_mut_ptr() as *mut GLchar);
            return Err(CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned())
        }

        Ok(program)
    }
}

fn main() {
    // TODO: Use env SCREEN or whatever
    let screenshot = get_screenshot(0).unwrap();

    if gtk::init().is_err() {
        println!("Failed to initialize GTK.");
        return;
    }

    // Set up window
    let window = gtk::Window::new(gtk::WindowType::Popup);
    window.set_name("lockscreen");
    // window.set_type_hint(gdk::WindowTypeHint::Dialog);
    window.set_decorated(false);
    window.set_app_paintable(true);

    // Get primary screen geometry
    let screen = window.get_screen().unwrap();
    let monitor_id = screen.get_primary_monitor();
    let monitor = screen.get_monitor_geometry(monitor_id);

    window.move_(0, 0);
    window.set_size_request(screen.get_width(), screen.get_height());

    // Set up styles
    let style_context = window.get_style_context().unwrap();
    let css_provider = gtk::CssProvider::new();
    let _ = css_provider.load_from_data("* { background-color: rgba(27, 29, 31, 0.8); }");
    style_context.add_provider(&css_provider, gtk::STYLE_PROVIDER_PRIORITY_APPLICATION);

    // Set up pattern widget
    let widget = gtk::DrawingArea::new();
    let widget_size = monitor.height / 3;
    widget.set_size_request(widget_size, widget_size);

    // Determine the trigger areas for the digits
    let margin  = widget_size as f64 * 0.1;
    let padding = widget_size as f64 * 0.1;
    let radius  = (widget_size as f64 - padding * 2.0 - margin * 2.0) / 6.0;
    let start   = margin + radius;
    let offset  = padding + radius * 2.0;

    let mut areas: [Circle; 9] = Default::default();
    for x in 0..3 {
        for y in 0..3 {
            let n = x + y * 3;
            areas[n].position.0 = start + offset * x as f64;
            areas[n].position.1 = start + offset * y as f64;
            areas[n].radius     = radius;
        }
    }

    let pattern_data = Rc::new(RefCell::new(PatternData { digit_areas: areas, ..Default::default() }));

    // Connect events
    widget.connect_draw(clone!(                pattern_data => move |widget, cx|    draw_pattern(&pattern_data, widget, cx)));
    widget.connect_button_press_event(clone!(  pattern_data => move |widget, event| handle_button_press(&pattern_data, widget, event)));
    widget.connect_motion_notify_event(clone!( pattern_data => move |widget, event| handle_motion_notify(&pattern_data, widget, event)));
    widget.connect_button_release_event(clone!(pattern_data => move |widget, event| handle_button_release(&pattern_data, widget, event)));

    // Tell gtk we actually want to receive the events
    let mut events = widget.get_events();
    events |= gdk_sys::GDK_BUTTON_PRESS_MASK.bits() as i32;
    events |= gdk_sys::GDK_BUTTON_RELEASE_MASK.bits() as i32;
    events |= gdk_sys::GDK_POINTER_MOTION_MASK.bits() as i32;
    widget.set_events(events);

    // Background
    let glarea = gtk::GLArea::new();
    glarea.set_size_request(screen.get_width(), screen.get_height());

    epoxy::load_with(|s| {
        unsafe {
            match shared_library::dynamic_library::DynamicLibrary::open(None).unwrap().symbol(s) {
                Ok(v) => v,
                Err(_) => ptr::null(),
            }
        }
    });
    gl::load_with(epoxy::get_proc_addr);

    let mut time_loc: Cell<GLint> = Cell::new(0);
    let mut program: Cell<GLuint> = Cell::new(0);

    glarea.connect_realize(clone!(glarea, time_loc, program => move |_| {
        glarea.make_current();

        let vertices: [GLfloat; 24] = [
            -1.0, -1.0, 0.0, 1.0,
             1.0,  1.0, 1.0, 0.0,
             1.0, -1.0, 1.0, 1.0,
            -1.0, -1.0, 0.0, 1.0,
             1.0,  1.0, 1.0, 0.0,
            -1.0,  1.0, 0.0, 0.0,
        ];

        let vert_shader_src = r#"
            #version 140
            in vec2 position;
            in vec2 tex_coords;
            out vec2 v_tex_coords;

            void main() {
                v_tex_coords = tex_coords;
                gl_Position = vec4(position, 0.0, 1.0);
            }
        "#;

        let frag_shader_src = r#"
            #version 140
            in vec2 v_tex_coords;
            out vec4 color;

            uniform float time;
            uniform sampler2D tex;

            void main() {
                vec4 pixel = texture(tex, v_tex_coords);
                float t = min(time / 2, 1.0);
                float tween = 1 - pow(-t + 1, 4.0);

                float luminance = dot(pixel.rgb, vec3(0.3, 0.59, 0.11));
                float new_lum = (1 - pow(1 - luminance, 3)) * 0.4;
                vec3 tint = vec3(0.21, 0.27, 0.35);

                vec4 lock_pixel = vec4(mix(vec3(new_lum), tint, 0.2), 1.0);

                color = mix(pixel, lock_pixel, tween);
            }
        "#;

        let vert_shader = match compile_shader(vert_shader_src, epoxy::VERTEX_SHADER) {
            Ok(v) => v,
            Err(e) => { panic!("Error compiling vertex shader: {}", e) },
        };
        let frag_shader = match compile_shader(frag_shader_src, epoxy::FRAGMENT_SHADER) {
            Ok(v) => v,
            Err(e) => { panic!("Error compiling fragment shader: {}", e) },
        };
        program.set(match link_program(vert_shader, frag_shader) {
            Ok(v) => v,
            Err(e) => { panic!("Error linking shader: {}", e) },
        });
        let program = program.get();

        let mut vao: GLuint = 0;
        let mut vbo: GLuint = 0;
        let mut tex: GLuint = 0;

        unsafe {
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);

            gl::GenBuffers(1, &mut vbo);
            gl::BindBuffer(epoxy::ARRAY_BUFFER, vbo);
            gl::BufferData(epoxy::ARRAY_BUFFER,
                           (vertices.len() * mem::size_of::<GLfloat>()) as GLsizeiptr,
                           &vertices[0] as *const f32 as *const _,
                           epoxy::STATIC_DRAW);

            gl::GenVertexArrays(1, &mut tex);
            gl::ActiveTexture(epoxy::TEXTURE0);
            gl::BindTexture(epoxy::TEXTURE_2D, tex);
            gl::TexImage2D(epoxy::TEXTURE_2D, 0, epoxy::RGB as GLint, screenshot.width() as GLint, screenshot.height() as GLint,
                           0, epoxy::BGRA, epoxy::UNSIGNED_BYTE, screenshot.get_data().as_ptr() as *const GLvoid);

            gl::TexParameteri(epoxy::TEXTURE_2D, epoxy::TEXTURE_WRAP_S, epoxy::CLAMP_TO_EDGE as GLint);
            gl::TexParameteri(epoxy::TEXTURE_2D, epoxy::TEXTURE_WRAP_T, epoxy::CLAMP_TO_EDGE as GLint);
            gl::TexParameteri(epoxy::TEXTURE_2D, epoxy::TEXTURE_MIN_FILTER, epoxy::NEAREST as GLint);
            gl::TexParameteri(epoxy::TEXTURE_2D, epoxy::TEXTURE_MAG_FILTER, epoxy::NEAREST as GLint);

            gl::UseProgram(program);
            gl::BindFragDataLocation(program, 0, b"color\0".as_ptr() as *const GLchar);

            let pos_attr = gl::GetAttribLocation(program, b"position\0".as_ptr() as *const GLchar);
            gl::EnableVertexAttribArray(pos_attr as GLuint);
            gl::VertexAttribPointer(pos_attr as GLuint, 2, epoxy::FLOAT, epoxy::FALSE as GLboolean,
                                    (4 * mem::size_of::<GLfloat>()) as GLint,
                                    ptr::null());

            let tex_attr = gl::GetAttribLocation(program, b"tex_coords\0".as_ptr() as *const GLchar);
            gl::EnableVertexAttribArray(tex_attr as GLuint);
            gl::VertexAttribPointer(tex_attr as GLuint, 2, epoxy::FLOAT, epoxy::FALSE as GLboolean,
                                    (4 * mem::size_of::<GLfloat>()) as GLint,
                                    (2 * mem::size_of::<GLfloat>()) as *const GLvoid);

            time_loc.set(gl::GetUniformLocation(program, b"time\0".as_ptr() as *const GLchar));
        }
    }));

    let start_time = std::time::Instant::now();
    glarea.connect_render(clone!(glarea, time_loc, program => move |_, _| {
        let t = start_time.elapsed().as_secs() as f32 + start_time.elapsed().subsec_nanos() as f32 / 1000000000.0;
        let time_loc = time_loc.get();

        unsafe {
            gl::Uniform1f(time_loc, t);

            gl::ClearColor(0.11, 0.12, 0.13, 1.0);
            gl::Clear(epoxy::COLOR_BUFFER_BIT);

            gl::DrawArrays(epoxy::TRIANGLES, 0, 6);
        };

        glarea.queue_draw();
        Inhibit(false)
    }));

    // Image
    let image_buffer = gdk_pixbuf::Pixbuf::new_from_file_at_scale("kuroko.png", -1, monitor.height * 2 / 3, true).unwrap();
    let image = gtk::Image::new_from_pixbuf(Some(&image_buffer));

    // Put everything together
    let container = gtk::Fixed::new();
    container.put(&glarea, 0, 0);
    container.put(&widget, monitor.x + monitor.width / 5, monitor.y + monitor.height / 3);
    container.put(&image, monitor.x + monitor.width / 2, monitor.y + monitor.height / 3);
    window.add(&container);
    window.show_all();

    // Grab input
    let gdk_window = window.get_window().unwrap();
    let display = screen.get_display();
    let device_manager = display.get_device_manager().unwrap();
    let pointer = device_manager.get_client_pointer();
    let keyboard = pointer.get_associated_device().unwrap();
    let cursor = gdk::Cursor::new_for_display(&display, gdk_sys::GdkCursorType::LeftPtr);

    window.connect_visibility_notify_event(move |_, _| {
        let _ = pointer.grab(&gdk_window, gdk::GrabOwnership::Application, true, gdk::EventMask::empty(),
                             &cursor, gdk_sys::GDK_CURRENT_TIME as u32);

        let _ = keyboard.grab(&gdk_window, gdk::GrabOwnership::Application, true, gdk::EventMask::empty(),
                              &cursor, gdk_sys::GDK_CURRENT_TIME as u32);

        Inhibit(false)
    });

    // Get ready to start
    window.connect_delete_event(|_, _| {
        gtk::main_quit();

        Inhibit(false)
    });

    gtk::main();
}

fn handle_button_press(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, event: &gdk::EventButton) -> gtk::Inhibit {
    let mut state = state.borrow_mut();
    state.reset_input();
    state.drawing = true;
    state.draw_position = event.get_position();

    widget.queue_draw();

    Inhibit(false)
}

fn handle_motion_notify(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, event: &gdk::EventMotion) -> gtk::Inhibit {
    let mut state = state.borrow_mut();
    if !state.drawing { return Inhibit(false) }

    let position = event.get_position();
    state.draw_position = position;

    for (index, area) in state.digit_areas.clone().into_iter().enumerate() {
        let digit = (index + 1) as u8;

        if area.contains(position) && state.can_input(digit) {
            state.input(digit);
        }
    }

    widget.queue_draw();

    Inhibit(false)
}

fn handle_button_release(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, event: &gdk::EventButton) -> gtk::Inhibit {
    let mut state = state.borrow_mut();
    state.drawing = false;

    widget.queue_draw();

    if state.current_input == [2, 4, 5, 6, 0, 0, 0, 0, 0] {
        gtk::main_quit();
    }

    Inhibit(false)
}

fn draw_pattern(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, context: &cairo::Context) -> gtk::Inhibit {
    let state = state.borrow();

    for (index, area) in state.digit_areas.into_iter().enumerate() {
        let digit = (index + 1) as u8;
        let (cx, cy) = area.position;

        context.set_source_rgba(1.0, 1.0, 1.0, 0.2);
        context.set_line_width(6.0);
        context.arc(cx, cy, area.radius * 1.0, -PI, PI);
        context.stroke();

        if state.input_contains(digit) {
            context.set_source_rgb(0.9, 0.9, 0.9);
            context.set_line_width(10.0);
            context.arc(cx, cy, area.radius * 0.1, -PI, PI);
            context.stroke();
        } else {
            context.set_source_rgb(0.0, 0.0, 0.0);
            context.set_line_width(6.0);
            context.arc(cx, cy, area.radius * 0.1, -PI, PI);
            context.stroke();

            context.set_source_rgb(0.8, 0.8, 0.8);
            context.set_line_width(2.0);
            context.arc(cx, cy, area.radius * 0.1, -PI, PI);
            context.stroke();
        }
    }

    context.set_source_rgb(0.9, 0.9, 0.9);
    context.set_line_width(4.0);
    context.set_line_cap(cairo::LineCap::Round);
    context.set_line_join(cairo::LineJoin::Round);

    // let mut code = String::with_capacity(20);
    // code.push_str("Code: ");

    for &digit in &state.current_input {
        if digit == 0 { break }
        // code.push((digit + 0x30) as char);

        let (sx, sy) = state.digit_areas[(digit - 1) as usize].position;

        context.line_to(sx, sy);
    }

    if state.drawing {
        context.line_to(state.draw_position.0, state.draw_position.1);
    }

    context.stroke();

    // context.set_font_size(12.0);
    // context.select_font_face("Droid Sans Mono",
    //                          cairo::enums::FontSlant::Normal,
    //                          cairo::enums::FontWeight::Normal);

    // context.move_to(0.0, 20.0);
    // context.set_source_rgba(1.0, 1.0, 1.0, 0.95);
    // context.show_text(&code);

    Inhibit(false)
}
