unit inventory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  EItemType = (item_weapon, item_armor, item_potion);

  { TItem }
  TItem = class;
  ItemList = specialize TList<TItem>;

  TItem = class
  private
    FType: EItemType;
    FName: string;
    FStackable: boolean;
    FCount: integer;
  public
    constructor Create(OType: EItemType; OName: string);
    destructor Destroy; override;
    property ItemType: EItemType read FType write FType;
    property ItemName: string read FName write FName;
    property Stackable: boolean read FStackable write FStackable;
    property ITemCount: integer read FCount write FCount;
  end;

  { TInventory }

  TInventory = class
  private
    FItems: ItemList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TItem }

constructor TItem.Create(OType: EItemType; OName: string);
begin
  Self.FType := OType;
  Self.FName := OName;
end;

destructor TItem.Destroy;
begin
  inherited Destroy;
end;

{ TInventory }

constructor TInventory.Create;
begin
  Self.FItems := ItemList.Create;
end;

destructor TInventory.Destroy;
var
  I: integer;
begin
  for I := 0 to Self.FItems.Count - 1 do
  begin
    Self.FItems[I].Free;
  end;
  FreeAndNil(Self.FItems);

  inherited Destroy;
end;

end.
