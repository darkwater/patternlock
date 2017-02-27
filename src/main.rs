extern crate gtk;
extern crate gdk;
extern crate gdk_sys;
extern crate gdk_pixbuf;
extern crate cairo;
extern crate screenshot;
extern crate image;

use std::rc::Rc;
use std::cell::RefCell;
use std::f64::consts::PI;
use gtk::prelude::*;
use screenshot::{Screenshot, get_screenshot};

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

    let mut events = widget.get_events();
    events |= gdk_sys::GDK_BUTTON_PRESS_MASK.bits() as i32;
    events |= gdk_sys::GDK_BUTTON_RELEASE_MASK.bits() as i32;
    events |= gdk_sys::GDK_POINTER_MOTION_MASK.bits() as i32;
    widget.set_events(events);

    // Background
    let bg_rgba: Vec<u8> = screenshot.get_data();
    let bg_pixbuf = gdk_pixbuf::Pixbuf::new_from_vec(bg_rgba, 0, true, 8, screenshot.width() as i32, screenshot.height() as i32, screenshot.width() as i32 * 4);
    let bg = gtk::Image::new_from_pixbuf(Some(&bg_pixbuf));

    // Image
    let image_buffer = gdk_pixbuf::Pixbuf::new_from_file_at_scale("kuroko.png", -1, monitor.height * 2 / 3, true).unwrap();
    let image = gtk::Image::new_from_pixbuf(Some(&image_buffer));

    let container = gtk::Fixed::new();
    container.put(&bg, 0, 0);
    container.put(&widget, monitor.x + monitor.width / 5, monitor.y + monitor.height / 3);
    container.put(&image, monitor.x + monitor.width / 2, monitor.y + monitor.height / 3);
    window.add(&container);
    window.show_all();

    let gdk_window = window.get_window().unwrap();
    let display = screen.get_display();
    let device_manager = display.get_device_manager().unwrap();
    let pointer = device_manager.get_client_pointer();
    let keyboard = pointer.get_associated_device().unwrap();
    let cursor = gdk::Cursor::new_for_display(&display, gdk_sys::GdkCursorType::LeftPtr);

    window.connect_visibility_notify_event(move |_, a| {
        let _ = pointer.grab(&gdk_window, gdk::GrabOwnership::Application, true, gdk::EventMask::empty(),
                             &cursor, gdk_sys::GDK_CURRENT_TIME as u32);

        let _ = keyboard.grab(&gdk_window, gdk::GrabOwnership::Application, true, gdk::EventMask::empty(),
                              &cursor, gdk_sys::GDK_CURRENT_TIME as u32);

        Inhibit(false)
    });

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
