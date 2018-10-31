--
--
--  Ada Spec: Generic_Graphs.Dynamic
--
--  Description: A "dynamic" realisation of graphs as definde in the parent.
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2006
--
--  Copyright: See COPYING file that comes with this distribution
--

with Portage.Ebuilds;
with Portage.UseFlags;

with Ada.Calendar;
--  with Ada.Finalization;

generic
	type T is abstract tagged limited private;
package Generic_Graphs.Dynamic is

	Not_Implemented : exception;

	type Dynamic_Graph is new T and Graph_Type with private;

	--  primitive operations
	overriding
	procedure Clear (graph : in out Dynamic_Graph);

	overriding
    procedure AddNode (graph : in out Dynamic_Graph;
        Node       : in Node_Type;
        Clear_Arcs : Boolean := False);

	overriding
    function HasNode (graph : Dynamic_Graph; Node : Node_Type) return Boolean;


	overriding
    procedure AddArc (graph : in out Dynamic_Graph;
        From, To : in Node_Type;
		arcLoad  : in Arc_Type);

	overriding
    function  HasArc (graph : Dynamic_Graph;
        From, To : Node_Type) return  Boolean;

	overriding
    function Arc (graph : Dynamic_Graph; From, To : Node_Type) return Arc_Type;

private

	--  Controlled will need to be added as a field, if needed
	type Dynamic_Graph is new T and Graph_Type with null record;


end Generic_Graphs.Dynamic;
