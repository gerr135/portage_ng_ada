procedure BC.Graphs.Directed.BFS_Traverse (G : Graph ; V : Vertex;
	Visited : in out B3C.Abstract_Collection'Class;
	Storage_Queue : in out BCQ.Abstract_Queue'Class;
	Visit_Incoming : Boolean := False )  is

	use B3C; use BCQ;

	--readability wrapper
	function Is_in(V:Vertex;
		Collection : B3C.Abstract_Collection'Class) return Boolean is
	begin
		return Location(C=>Collection,Elem=>V) /= 0;
	end;
	pragma Inline(Is_in);

	-- direction choice
	function New_Iterator(For_The_Vertex:Vertex) return Vertex_Iterator'Class is
	begin
		if Visit_Incoming then
			return New_Vertex_Incoming_Iterator(For_The_Vertex);
		else
			return New_Vertex_Outgoing_Iterator(For_The_Vertex);
		end if;
	end;
	pragma Inline(New_Iterator);

	CheckOk : Boolean := True;

begin
	Apply(V, Ok=>CheckOk);
	Append(C=>Visited, Elem=>V);
	Append(Q=>Storage_Queue, Elem=>V);
	if not CheckOk then return; end if;

	while not Is_Empty(Storage_Queue) loop
	  declare
		Source   : Vertex := Front(Storage_Queue);
		Iterator : Vertex_Iterator'Class :=
			New_Iterator(For_The_Vertex=>Source);
	  begin
		Pop(Storage_Queue);
		while not Is_Done(Iterator) loop
		  declare
		  	Dest : Vertex;
		  begin
			if Visit_Incoming then
				From_Vertex(Arc(Current_Arc(Iterator)),Dest);
			else
				To_Vertex(Arc(Current_Arc(Iterator)),Dest);
			end if;
			if Arc_Valid(Current_Arc(Iterator)) and then
				not Is_in(V=>Dest, Collection=>Visited)
			then
				Apply(Dest, Ok=>CheckOk);
				Append(C=>Visited, Elem=>Dest);
				Append(Q=>Storage_Queue, Elem=>Dest);
				if not CheckOk then return; end if;
			end if;
			Next(Iterator);
		  end;
		end loop;
	  end;
	end loop;
end BC.Graphs.Directed.BFS_Traverse;

