from core::char import all;
from core::io import all;
from core::text import all;

module Input {
    public func prompt_line(msg: String) -> IO String {
        do {
            print(msg);
            io::stdout::flush();
            io::stdin::get_line();
        }
    }

    public func input<Read A>(msg: String) -> IO (Maybe A) {
        text::read_maybe() <$> prompt_line(msg)
    }

    public func prompt<Read A>(msg: String, validator: A -> Bool) -> IO A {
        do {
            let res <- input(msg)
            
            match res {
                Just(x) => {
                    if validator(x) then return x else repeat
                }
                Nothing => repeat()
            }
        } where {
            repeat: {
                println("Invalid input! Please try again.")
                ditto(msg, validator)
            }
        }
    }

    public func prompt_int(msg: String, validator: Int -> Bool) -> IO Int {
        prompt(msg, validator)
    }

    public func yes_or_no(msg: String) -> IO Bool {
        do {
            let str <- prompt_line(msg ++ " (y / n): ")
            
            match (to_lower `map` str) {
                ('y' : _) => return true
                ('n' : _) => return false
                _ => repeat
            }
       } where {
            repeat: {
                println("Invalid input! Please try again.")
                ditto(msg)
            }
        }
    }
}