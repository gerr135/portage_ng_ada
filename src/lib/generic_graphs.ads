--
--  Ada Spec: Generic_Graphs
--
--  Description: Graph essentials, portage independent interface.
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2006-2008
--
--  Copyright: See COPYING file that comes with this distribution
--

generic
    type Node_Type is private;
    with function "=" (Left, Right : Node_Type) return Boolean is <>;
    type Arc_Type is private;
package Generic_Graphs is

    type Graph_Type is limited interface;

    --  primitive operations
    procedure Clear (graph : in out Graph_Type) is abstract;

    --  Add new (not connected) node.
    procedure AddNode
       (graph      : in out Graph_Type;
        Node       : in Node_Type;
        Clear_Arcs : Boolean := False) is abstract;
    --  If such node (with a given element) already exists then
    --  if Clear_Arcs then remove all arcs
    --  else do nothing

    function HasNode (graph : Graph_Type; Node : Node_Type) return Boolean is abstract;

    --  Add arc. Again, there can be only one between two nodes.
    --  NOTE! Arcs are directed!
    procedure AddArc
       (graph    : in out Graph_Type;
        From, To : in Node_Type;
        arcLoad  : in Arc_Type) is abstract;

    function HasArc
       (graph    : Graph_Type;
        From, To : Node_Type)
        return     Boolean is abstract;

    function Arc (graph : Graph_Type; From, To : Node_Type) return Arc_Type is abstract;

end Generic_Graphs;
