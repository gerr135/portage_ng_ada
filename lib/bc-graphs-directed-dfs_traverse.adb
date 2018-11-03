
--generic
--with procedure Apply(VElem : in Abstract_Vertex'Class;OK : out Boolean);
--with function Arc_Valid(AElem : in Abstract_Arc'Class) return Boolean;
procedure BC.Graphs.Directed.DFS_Traverse (G : Graph ; V : Vertex;
		Visited : in out B3C.Abstract_Collection'Class ) is

NotOk : Exception;
-- terminating traversal upon request.
-- seems easiest to do with exception, since traversal is recoursive
CheckOk : Boolean := True;

	procedure Do_Traverse(Start:Vertex) is
		Iterator : Vertex_Iterator'Class :=
			New_Vertex_Outgoing_Iterator(For_The_Vertex=>Start);

		use B3C;

		--readability boolean
		function Is_in(V:Vertex;
			Collection : B3C.Abstract_Collection'Class) return Boolean is
		begin
			return Location(C=>Collection,Elem=>V) /= 0;
		end;
		pragma Inline(Is_in);


	begin
		-- visit passed vertex
		Apply(Start, Ok=>CheckOk);
		Append(C=>Visited, Elem=>Start);
		if not CheckOk then raise NotOk; end if;
		-- and then iterate over outgoing. Iterator is reset at construction
		while not Is_Done(Iterator) loop
			declare
				curV : Vertex;
			begin
				To_Vertex(Arc(Current_Arc(Iterator)),curV);
				if Arc_Valid(Current_Arc(Iterator)) and then
					not Is_in(V=>curV, Collection=>Visited)
				then
					Do_Traverse(Start => curV);
				end if;
				Next(Iterator);
			end;
		end loop;
	end;

begin
	B3C.Clear(Visited);
	Do_Traverse(Start=>V);
	exception
		when NotOk =>
			null;
			-- everything is cleaned up already, normal termination
end;
