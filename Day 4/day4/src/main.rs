use dialoguer::Input;
use indicatif::ProgressBar;

fn main() -> Result<(), std::io::Error> {
    let input: String = Input::new()
        .with_prompt("Input: ")
        .default("aaaaaaaa".into())
        .interact_text()?;
    let spinner = ProgressBar::new_spinner();
    println!("{}", run_tests(input, &spinner));
    Ok(())
}

fn run_tests(input: String, spinner: &ProgressBar) -> u64 {
    spinner.inc(1);
    let mut num: u64 = 0;
    let mut hash: String = get_md5_sring(&input, num);
    spinner.set_message(num.to_string());
    while &hash[0..5] != "00000" {
        spinner.inc(1);
        num += 1;
        hash = get_md5_sring(&input, num);
        spinner.set_message(num.to_string());
    }
    return num;
}

fn get_md5_sring(input: &String, num: u64) -> String {
    format!("{:x}", md5::compute(format!("{}{}", input, num.to_string().as_str())))
}
