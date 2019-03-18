use super::*;
use input::*;
use input::staticinfrastructure::*;

use std;
use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;


#[no_mangle]
pub unsafe extern fn parse_infrastructure_file(filename :*const c_char) 
    -> *mut (StaticInfrastructure,InfNames<String>) {
    let filename =  CStr::from_ptr(filename).to_str().unwrap();
    println!("rolling ffi: loading {}",filename);
    match get_infrastructure(Path::new(filename)) {
        Ok(inf) => Box::into_raw(Box::new(inf)),
        Err(e) => {
            println!("Error parsing infrastructure: {}", e);
            std::ptr::null_mut()
        }
    }
}


#[no_mangle]
pub unsafe extern fn parse_routes_file(inf :*mut (StaticInfrastructure, InfNames<String>), 
                                       filename :*const c_char) 
    -> *mut Routes<String> {
    let filename =  CStr::from_ptr(filename).to_str().unwrap();
    match get_routes(Path::new(filename), &((*inf).1)) {
        Ok(routes) => Box::into_raw(Box::new(routes)),
        Err(e) => {
            println!("Error parsing routes: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern fn parse_dispatch(input :*const c_char) -> *mut dispatch::Dispatch<String> {
    let input = CStr::from_ptr(input).to_str().unwrap();
    match dispatch::parse_dispatch(input) {
        Ok(dispatch) => Box::into_raw(Box::new(dispatch)),
        Err(e) => {
            println!("Error parsing dispatch plan: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern fn eval_simplified(inf :*mut (StaticInfrastructure, InfNames<String>),
                                    routes: *mut Routes<String>,
                             dis :*mut dispatch::Dispatch<String>) -> *mut c_char {
    let result = evaluate_plan(&((*inf).0),&*routes,&*dis, None);
    let result = output::history::visits(&((*inf).1), &result);

    match result {
        Ok(string) => {
          let x = CString::new(string).unwrap();
          x.into_raw()
        },
        Err(e) => {
            println!("Error evaluating plan: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern fn free_string(s :*mut c_char) {
    println!("freeing string");
     CString::from_raw(s); 
}

#[no_mangle]
pub unsafe extern fn free_infrastructure(x :*mut StaticInfrastructure) {
    println!("Freeing infrastructure");
    Box::from_raw(x);
}

#[no_mangle]
pub unsafe extern fn free_routes(x :*mut Routes<String>) {
    println!("freeing routes");
    Box::from_raw(x);
}

#[no_mangle]
pub unsafe extern fn free_dispatch(x :*mut dispatch::Dispatch<String>) {
    println!("freeing dispatch");
    Box::from_raw(x);
}

