-ifdef(TEST).
-define(LOG_INFO(F), _ = [F]).
-define(LOG_ERROR(F), _ = [F]).
-define(LOG_INFO_FORMAT(F, A), _ = [F, A]).
-define(LOG_ERROR_FORMAT(F, A), _ = [F, A]).
-else.
-define(LOG_INFO(F),
	error_logger:info_msg(F)).
-define(LOG_ERROR(F),
	error_logger:error_msg(F)).
-define(LOG_INFO_FORMAT(F, A),
	error_logger:info_msg(F, A)).
-define(LOG_ERROR_FORMAT(F, A),
	error_logger:error_msg(F, A)).
-endif.
