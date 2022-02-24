extern crate md5;

fn main() {
    let input = "yzbqklnj";
}

fn process_input(key: &str) {
    let n = 0_usize;
    
    loop {
        let curput: String = key.to_string().add(n.to_string().as_str());
        let digest = md5::compute(curput.into_bytes());
        n += 1;
    }
}

