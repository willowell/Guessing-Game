// promptLine :: String -> IO String
func promptLine(msg: String) -> Optional<String> {
    print(msg, terminator: "")
    return readLine()
}

// Text.Read.readMaybe :: Read a => String -> Maybe a
func read<T>(_ str: String, as type: T.Type) -> Optional<T> where T: LosslessStringConvertible {
    type.init(str)
}

// input :: Read a => String -> IO (Maybe a)
func input<T>(msg: String) -> Optional<T> where T: LosslessStringConvertible {
    if case .some(let s) = promptLine(msg: msg) {
       return read(s, as: T.self)
    } else {
        return .none
    }
}

// prompt :: Read a => String -> (a -> Bool) -> IO a
func prompt<T>(msg: String, validator: (T) -> Bool) -> T where T: LosslessStringConvertible {
    while true {
        if case .some (let val) = input(msg: msg) as T? {
            if validator(val) {
                return val
            } else {
                print("Invalid input! Please try again.")
            }
        } else {
            print("Invalid input! Please try again.")
            continue
        }
    }
}

func promptInt(msg: String, validator: (Int) -> Bool) -> Int {
    prompt(msg: msg, validator: validator)
}

func promptDouble(msg: String, validator: (Double) -> Bool) -> Double {
    prompt(msg: msg, validator: validator)
}

func yesOrNo(msg: String) -> Bool {
    while true {
        let str = promptLine(msg: (msg + " (y / n): "))
        if case .some(let val) = str {
            switch val.prefix(1) {
                case "y":
                    return true
                case "n":
                    return false
                default:
                    print("Invalid input! Please try again.")
                    continue
            }
        } else {
            print("Invalid input! Please try again.")
            continue
        }
    }
}