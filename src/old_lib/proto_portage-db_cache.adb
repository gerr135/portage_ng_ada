with Proto_Portage.Support; use Proto_Portage.Support;
with BC.Graphs.Directed.BFS_Traverse;
with GNAT.Directory_Operations.Iteration;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Proto_Portage.DB_Cache is

use AG, DG;

-- Graphs in Booch components operate on Vertices that
-- *contain* items. That is, different vertices can contain
-- the same item.
-- We have an 1:1 mapping in our case, and we really want to
-- operate on items.
-- Thus we need a utility function that returns vertex for a given item..

procedure Vertex_At(graph : DG.Graph; key : Ebuild_Key;
		V : out Vertex; Found : out Boolean) is
	-- since the underlying graph is a bit too abstract the only
	-- way to get a Vertex is to search through the graph..
	-- But this is only the prototype code, in real one we can
	-- have more efficient representation..

	Iter : Graph_Iterator'Class := New_Graph_Iterator (For_The_Graph=>graph);
		-- reset at creation
begin
	Found := False;
	--Put("vertex for " & Get_Name(key) & ": ");
	while not Is_Done(Iter) loop
		--Put(Get_Name(Item(Current_Vertex(Iter)))&", ");
		if Item(Current_Vertex(Iter)) = key then
			V := Vertex(Current_Vertex(Iter));
			Found := True;
			--Put_Line("successfully completed request for the key: "&
			--	Get_Name(key));
			exit;
		end if;
		Next(Iter);
	end loop;
	--New_Line;
end;



----------------------------------

procedure Add_Dependency (Cache : in out Dependency_Cache;
		From_Key, To_Key : Ebuild_Key;
		Condition : Useflag_Set;
		Outstanding_Keys : in out Ebuild_Key_Set) is

	A : Arc;

	function Protected_VertexAt(key : Ebuild_Key) return Vertex is
		V : Vertex;
		Found : Boolean;
	begin
		Vertex_At(Cache.graph,key,V,Found);
		if not Found then
			Create_Vertex(G=>Cache.graph, V=>V, I=>key);
				-- add key to vertex
			Add_Key(Outstanding_Keys, key);
				-- and register as missing
		end if;
		return V;
	end;

	VFrom : Vertex := Protected_VertexAt(From_Key);
	VTo   : Vertex := Protected_VertexAt(To_Key);
begin

	Create_Arc(Cache.graph, A, I=>Condition,
		From=>VFrom, To=>VTo );
end Add_Dependency;


procedure Add_Key (Cache : in out Dependency_Cache; key : Ebuild_Key;
		Outstanding_Keys : in out Ebuild_Key_Set) is
	V : Vertex;
	Found : Boolean := False;
begin
	-- try to look it up first
	Vertex_At(Cache.graph,key,V,Found);
	if not Found then -- and only then!
		Create_Vertex(G=>Cache.graph, V=>V, I=>key);
	end if;
	Remove_Key(TheSet=>Outstanding_Keys, key=>key);
end Add_Key;


procedure Clear (Cache : in out Dependency_Cache) is
begin
	Clear(Cache.Graph);
end Clear;
pragma Inline(Clear);


procedure Clear_All (Cache : in out Dependency_Cache) is
begin
	Clear(Cache);
	Cache.Cache_Path := Null_Unbounded_String;
	Cache.Tree_Path  := Null_Unbounded_String;
end Clear_All;


procedure Dump (Cache : in out Dependency_Cache; To_Path : String) is
begin
	Cache.Cache_Path := To_Unbounded_String(To_Path);
	Dump(Cache);
end Dump;


procedure Dump (Cache : in out Dependency_Cache) is
begin
	null;
end Dump;


function get_Cache_Location (Cache : Dependency_Cache) return String is
begin
	return To_String(Cache.Cache_Path);
end get_Cache_Location;



-------------------------
-- Get_Dependency_List --
-------------------------

function Get_Dependency_List  -- the "principal" method
	(Cache : Dependency_Cache;
		key : Ebuild_Key;
		Use_set : Useflag_Set;
		Direction : DepTraversal_Direction := Direct)
	return Ebuilds.List  is
	-- but an easy one in fact

	procedure Blank_Apply(VElem : in Abstract_Vertex'Class;OK : out Boolean) is
	begin Ok:=True; end;

	function Dependency_Valid(AElem : in Abstract_Arc'Class) return Boolean is
	begin
		return Is_aSubset(Subset => Useflag_Set(Item(AElem)) , Set=> Use_Set);
	end;

	--use B3C_EbColl; use BC_EbQueues; use AG,DG;

	procedure Traverse is new DG.BFS_Traverse(
		Apply => Blank_Apply,
		Arc_Valid => Dependency_Valid,
		BCC=>DG_EbKeys,
		B3C=>DG_EbColl,
		BCQ=>DG_EbQueues);

	-- done with instantiations, now local vars
	use DG_EbUMC;

	V : Vertex;
	Vertex_Found : Boolean;

	DepList : Ebuilds.List;
	DepCollection : Collection;
	TmpQueue : DG_EbQUM.Queue;
	Visit_Incoming : Boolean := False;

begin
	--Put_Line("graph capacity: " & Number_Of_Vertices(Cache.graph)'Img);
	-- that's the only debug feedback hook I needed here!

	Vertex_At(Cache.graph, key, V, Vertex_Found);
	if not Vertex_Found then
		raise Key_Not_Found;
	end if;

	if Direction = Reversed then
		Visit_Incoming := True;
	end if;

	Traverse(G=>Cache.graph, V=>V,
		Visited=>DepCollection,
		Storage_Queue => TmpQueue,
		Visit_Incoming => Visit_Incoming);

	-- now we need to convert DepCollection into DepList
	--
	-- It is possible to organize global structure so as to
	-- bundle lists together with db-cache structure
	-- in the parent and get access to list's internal fields in the *body* of this
	-- package, but I chose a path of better separation. This way we can change
	-- the internals of Generic_Lists (and thus DepList) at any time and we will
	-- need to do exactly zero modifications to this code!
	--
	-- Besides this is easy to do and is hardly a bottleneck performance-wise,
	-- plus anywayit is nice to reverse the list, as BFS_Traverse returns
	-- dependensies in descending order (dependants first).
	--
	-- Another besides :) : we need to reverse the order in case of direct
	-- traversal and return as-is in case of reverse traversal..
	-- (so that dependencies always go first)

	if Direction=Direct then
		for i in reverse 1 .. Length(DepCollection) loop
			-- will count from Length()  down to 1
			-- the Length() .. 1 is an empty range (quite handy at times)
			Ebuilds.Append(TheList=>DepList,
				key=>Item(Item_At(DepCollection, At_Index=>i)) );
		end loop;
	else
		for i in 1 .. Length(DepCollection) loop
			Ebuilds.Append(TheList=>DepList,
				key=>Item(Item_At(DepCollection, At_Index=>i)) );
		end loop;
	end if;

	return DepList;
end Get_Dependency_List;



function Get_Tree_Location (Cache : Dependency_Cache) return String is
begin
	return To_String(Cache.Tree_Path);
end Get_Tree_Location;


-----------------------------
-- Regen_From_TextTree

procedure Regen_From_TextTree (Cache : in out Dependency_Cache;
	Tree_Location : String; Debug : Boolean := False) is
begin
	Cache.Tree_Path := To_Unbounded_String(Tree_Location);
	Regen_From_TextTree(Cache, Debug);
end Regen_From_TextTree;

procedure Regen_From_TextTree (
		Cache : in out Dependency_Cache;
		Debug : Boolean := False) is              Separate;
	-- this procedure got too big, so I decided to put it in a separate file
	-- to enhance readability.
	-- Besides this will serve as a nice illustration of one more аda feature :)


procedure set_Cache_Location (Cache : in out Dependency_Cache;
	Path : String) is
begin
	Cache.Cache_Path := To_Unbounded_String(Path);
end set_Cache_Location;


procedure Set_Tree_Location (Cache : in out Dependency_Cache;
	Path : String) is
begin
	Cache.Tree_Path := To_Unbounded_String(Path);
end Set_Tree_Location;


procedure Suck (Cache : in out Dependency_Cache; From_Path : String) is
begin
	Cache.Cache_Path := To_Unbounded_String(From_Path);
	Suck(Cache);
end Suck;


procedure Suck (Cache : in out Dependency_Cache) is
begin
	raise Broken_Cache;
	-- just this here until I implement cache dumps and reads
end Suck;

end Proto_Portage.DB_Cache;

