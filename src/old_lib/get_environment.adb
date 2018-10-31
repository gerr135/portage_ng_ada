with Interfaces.C.Strings; use Interfaces.C.Strings;

function Get_Environment(Variable : String) return String is
-- Return the value of the given environment variable.
-- If there's no such environment variable, return an empty string.

	function Value_Without_Exception(S : chars_ptr) return String is
	-- Translate S from a C-style char* into an Ada String.
	-- If S is Null_Ptr, return "", don't raise an exception.
	begin
		if S = Null_Ptr then return "";
			else return Value(S);
		end if;
	end Value_Without_Exception;
	pragma Inline(Value_Without_Exception);


	function getenv(Variable : chars_ptr) return chars_ptr;
	pragma Import(C, getenv);
	-- getenv is a standard C library function; see K&R 2, 1988, page 253.
	-- it returns a pointer to the first character; do NOT free its results.

	Variable_In_C_Format : chars_ptr := New_String(Variable);
	Result_Ptr : chars_ptr := getenv(Variable_In_C_Format);
	Result : String := Value_Without_Exception(Result_Ptr);

begin
	Free(Variable_In_C_Format);
	return Result;
end Get_Environment;
