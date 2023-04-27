# encoding: utf-8


def theme(context):
    return context, [
        [
            r"^(Test suite [^:]+:) (RUNNING\.\.\.)?(PASS)?(FAIL)?$",
            "blue,white,green,red",
            "bold",
        ],
        [r"\r(### Failure in:) (.+)", "red,white", "underline,bold"],
        [
            r"\r(Cases: \d+\s+Tried: \d+)\s+(Errors: 0)?(Errors: [1-9]\d*)?\s+(Failures: 0)?(Failures: [1-9]\d*)?$",
            "white,green,red,green,red",
            "bold",
        ],
    ]
