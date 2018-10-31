package body Generic_Graphs.Dynamic is

    -----------
    -- Clear --
    -----------

    procedure Clear (graph : in out Dynamic_Graph) is
    begin
		raise Not_Implemented;
    end Clear;

    -------------
    -- AddNode --
    -------------

    procedure AddNode (graph      : in out Dynamic_Graph;
        Node       : in Node_Type; Clear_Arcs : Boolean := False) is
    begin
		raise Not_Implemented;
    end AddNode;

    -------------
    -- HasNode --
    -------------

    function HasNode (graph : Dynamic_Graph; Node : Node_Type) return Boolean is
    begin
		raise Not_Implemented;
        return False;
    end HasNode;

    ------------
    -- AddArc --
    ------------

    procedure AddArc (graph    : in out Dynamic_Graph;
        From, To : in Node_Type; arcLoad  : in Arc_Type) is
    begin
		raise Not_Implemented;
    end AddArc;

    ------------
    -- HasArc --
    ------------

    function HasArc (graph : Dynamic_Graph; From, To : Node_Type) return Boolean is
    begin
		raise Not_Implemented;
        return False;
    end HasArc;

    ---------
    -- Arc --
    ---------

    function Arc (graph : Dynamic_Graph; From, To : Node_Type) return Arc_Type is
		Arc : Arc_Type;
    begin
		raise Not_Implemented;
        return Arc;
    end Arc;

end Generic_Graphs.Dynamic;
