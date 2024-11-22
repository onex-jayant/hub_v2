%-define(oxlog_d(Format, Args), lager:log(debug, self(), Format, Args)).

-define(oxlog_i(Format, Args), lager:log(info, self(), Format, Args)).

-define(oxlog_e(Format, Args), lager:log(error, self(), Format, Args)).