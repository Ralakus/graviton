pub extern crate graviton_core as core;

fn main() {
    let source = "// code\n//code 2\n//code 3\n// some code 4\n// some more code 5\nlet a = 14;\n// yet more code 7\n// code 8\n// code 9\n// code 10";

    let from = String::from("Name Analyzer");
    let msg =
        String::from("\"a\" is not a good variable name due to being less than 3 characters long");
    let pos = core::Position::new(6, 5);
    let file = String::from("main.grav");

    let notice = core::Notice::new(
        from.clone(),
        msg.clone(),
        pos,
        file.clone(),
        core::NoticeLevel::Notice,
    );

    notice.report(Some(source));

    let notice = core::Notice::new(
        from.clone(),
        msg.clone(),
        pos,
        file.clone(),
        core::NoticeLevel::Warning,
    );

    notice.report(Some(source));

    let notice = core::Notice::new(from, msg, pos, file, core::NoticeLevel::Error);

    notice.report(Some(source));
}
