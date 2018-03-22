use input::*;

use std;
use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;
use std::path::Path;

#[no_mangle]
pub extern fn parse_infrastructure_file(filename :*const c_char) -> *mut staticinfrastructure::StaticInfrastructure {
    let filename = unsafe { CStr::from_ptr(filename) }.to_str().unwrap();
    match staticinfrastructure_parser::parse_file(Path::new(filename)) {
        // TODO manage namemaps
        Ok(inf) => Box::into_raw(Box::new(inf)),
        Err(e) => {
            println!("Error parsing infrastructure: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern fn parse_routes_file(filename :*const c_char) -> *mut Vec<staticinfrastructure::Route> {
    let filename = unsafe { CStr::from_ptr(filename) }.to_str().unwrap();
                                                        // TODO mangage name maps
    match route_parser::parse_file(Path::new(filename), panic!(), panic!()) {
        Ok(routes) => Box::into_raw(Box::new(routes)),
        Err(e) => {
            println!("Error parsing infrastructure: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern fn parse_dispatch(input :*const c_char) -> *mut dispatch::Dispatch {
    let input = unsafe { CStr::from_ptr(input) }.to_str().unwrap();
    match dispatch::parse_dispatch(input) {
        Ok(dispatch) => Box::into_raw(Box::new(dispatch)),
        Err(e) => {
            println!("Error parsing dispatch plan: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern fn create_history(inf :*mut staticinfrastructure::StaticInfrastructure,
                             dis :*mut dispatch::Dispatch) -> *mut c_char {
    let x = CString::new("hello").unwrap();
    x.into_raw()
}


/// One line containing timing of route activations
/// One line per train containing times when reaching nodes
/// This is the minimum of input required by the local capacity verification tool
#[no_mangle]
pub extern fn create_simplified_history(inf :*mut staticinfrastructure::StaticInfrastructure,
                             dis :*mut dispatch::Dispatch) -> *mut c_char {
    let x = CString::new("hello").unwrap();
    x.into_raw()
}

#[no_mangle]
pub extern fn free_history(s :*mut c_char) {
    unsafe { CString::from_raw(s); }
}

