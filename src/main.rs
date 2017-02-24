extern crate gtk;
extern crate gdk;
extern crate gdk_sys;
extern crate cairo;

use std::rc::Rc;
use std::cell::RefCell;
use std::f64::consts::PI;
use gtk::prelude::*;

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
#[derive(Default)]
struct PatternData {
    input: [u8; 9],
    next_digit: usize,
    drawing: bool,
    draw_position: (f64, f64),
    digit_areas: [Circle; 9]
}

fn main() {
    if gtk::init().is_err() {
        println!("Failed to initialize GTK.");
        return;
    }

    // Set up window
    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    window.set_name("lockscreen");
    window.set_type_hint(gdk::WindowTypeHint::Dialog);
    window.set_decorated(false);

    // Get primary screen geometry
    let screen = WindowExt::get_screen(&window).unwrap();
    let monitor_id = screen.get_primary_monitor();
    let monitor = screen.get_monitor_geometry(monitor_id);

    window.move_(monitor.x, monitor.y);
    window.set_size_request(monitor.width, monitor.height);
    window.fullscreen();

    // Set up styles
    let style_context = window.get_style_context().unwrap();
    let css_provider = gtk::CssProvider::new();
    let _ = css_provider.load_from_data("* { background: #1d1f21 url('kuroko.png') no-repeat 80% bottom; background-size: 600px 600px; }");
    style_context.add_provider(&css_provider, gtk::STYLE_PROVIDER_PRIORITY_APPLICATION);

    // Set up pattern widget
    let widget = gtk::DrawingArea::new();
    let widget_size = monitor.height / 3;
    widget.set_size_request(widget_size, widget_size);

    // Determine the trigger areas for the digits
    let margin  = widget_size as f64 * 0.1;
    let padding = widget_size as f64 * 0.05;
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

    let container = gtk::Fixed::new();
    container.put(&widget, monitor.width / 5, monitor.height / 3);
    window.add(&container);
    window.show_all();

    window.connect_delete_event(|_, _| {
        gtk::main_quit();

        Inhibit(false)
    });

    gtk::main();
}

fn handle_button_press(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, event: &gdk::EventButton) -> gtk::Inhibit {
    let mut state = state.borrow_mut();
    state.input = [0; 9];
    state.next_digit = 0;
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

        if area.contains(position) {
            if match state.next_digit {
                0 => true,
                9 => false,
                _ => state.input[state.next_digit - 1] != digit
            } {
                state.input[state.next_digit] = digit;
                state.next_digit += 1;
            }
        }

        // if state.next_digit == 0 {
        //     state.input[0] = digit;
        //     state.next_digit += 1;
        // } else if state.next_digit < 9 {
        //     if state.input[state.next_digit - 1] != digit {
        //         state.input[state.next_digit] = digit;
        //         state.next_digit += 1;
        //     }
        // }
    }

    widget.queue_draw();

    Inhibit(false)
}

fn handle_button_release(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, event: &gdk::EventButton) -> gtk::Inhibit {
    let mut state = state.borrow_mut();
    state.drawing = false;

    widget.queue_draw();

    if state.input == [1, 4, 8, 6, 3, 2, 0, 0, 0] {
        gtk::main_quit();
    }

    Inhibit(false)
}

fn draw_pattern(state: &RefCell<PatternData>, widget: &gtk::DrawingArea, context: &cairo::Context) -> gtk::Inhibit {
    let state = state.borrow();

    for area in state.digit_areas.into_iter() {
        let (cx, cy) = area.position;

        context.set_source_rgb(0.0, 0.0, 0.0);
        context.set_line_width(6.0);
        context.arc(cx, cy, area.radius * 0.1, -PI, PI);
        context.stroke();

        context.set_source_rgb(0.8, 0.8, 0.8);
        context.set_line_width(2.0);
        context.arc(cx, cy, area.radius * 0.1, -PI, PI);
        context.stroke();
    }

    context.set_source_rgb(0.9, 0.9, 0.9);

    for &digit in &state.input {
        if digit == 0 { break }

        let (sx, sy) = state.digit_areas[(digit - 1) as usize].position;

        context.line_to(sx, sy);
    }

    if state.drawing {
        context.line_to(state.draw_position.0, state.draw_position.1);
    }

    context.stroke();

    Inhibit(false)
}
