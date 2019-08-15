module MyFunction

export
hello : String -> String
hello req = "Hello: " ++ req

lib : FFI_Export FFI_JS "" []
lib =
    Fun hello "hello" $
    End