use super::*;
use input::*;
use input::staticinfrastructure::*;

use std;
use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;


#[no_mangle]
pub unsafe extern fn parse_infrastructure_file(filename :*const c_char) 
    -> *mut StaticInfrastructure {
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
pub unsafe extern fn parse_routes_file(inf :*mut StaticInfrastructure, 
                                       filename :*const c_char) 
    -> *mut Routes {
    let filename =  CStr::from_ptr(filename).to_str().unwrap();
    match get_routes(Path::new(filename), &*inf) {
        Ok(routes) => Box::into_raw(Box::new(routes)),
        Err(e) => {
            println!("Error parsing routes: {:?}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern fn parse_dispatch(input :*const c_char) -> *mut dispatch::Dispatch {
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
pub unsafe extern fn eval_simplified(inf :*mut StaticInfrastructure,
                                    routes: *mut Routes,
                             dis :*mut dispatch::Dispatch) -> *mut c_char {
    let result = evaluate_plan(&*inf,&*routes,&*dis);
    let result = output::history::visits(&*inf, &result);

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
     CString::from_raw(s); 
}

#[no_mangle]
pub unsafe extern fn free_infrastructure(x :*mut StaticInfrastructure) {
    Box::from_raw(x);
}

#[no_mangle]
pub unsafe extern fn free_routes(x :*mut Routes) {
    Box::from_raw(x);
}

#[no_mangle]
pub unsafe extern fn free_dispatch(x :*mut dispatch::Dispatch) {
    Box::from_raw(x);
}

