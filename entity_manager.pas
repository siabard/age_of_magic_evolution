unit entity_manager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, entity, Generics.Collections, LogUtil;

type
  TEntityManager = class
  private
    FEntities: specialize TList<TEntity>;
    FAddedEntities: specialize TList<TEntity>;
    entity_id: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddEntity(): TEntity;
    procedure Update();
    function GetEntities: specialize TList<TEntity>;
    function GetEntity(ATag: string): TEntity;
  end;

implementation

constructor TEntityManager.Create;
begin
  FEntities := specialize TList<TEntity>.Create;
  FAddedEntities := specialize TList<TEntity>.Create;
  entity_id := 0;
end;

destructor TEntityManager.Destroy;
var
  AEntity: TEntity;
  I: integer;
begin
  for I := 0 to FEntities.Count - 1 do
  begin
    AEntity := FEntities[I];
    if Assigned(AEntity) and (AEntity <> nil) then
    begin
      AEntity.Free;
    end;
  end;
  LogDebug('Free Entities');
  FEntities.Free;
end;

function TEntityManager.AddEntity(): TEntity;
var
  AEntity: TEntity;
begin
  Inc(entity_id);
  LogDebug('TEntityManager.AddEntity');
  AEntity := TEntity.Create;
  AEntity.setNid(entity_id);
  FAddedEntities.Add(AEntity);

  Result := AEntity;
end;

procedure TEntityManager.Update;
var
  AEntity: TEntity;
  SubEntities: specialize TList<TEntity>;
  ToDeletes: specialize TList<TEntity>;
begin

  // 삭제된 Entity 항목을 모두 지운다.
  SubEntities := specialize TList<TEntity>.Create;
  ToDeletes := specialize TList<TEntity>.Create;

  for AEntity in FEntities do
  begin
    if AEntity.IsLive = False then
    begin
      AEntity.Free;
    end
    else
    begin
      SubEntities.Add(AEntity);
    end;
  end;

  // 추가된 Entity 항목을 모두 더한다.
  for AEntity in FAddedEntities do
  begin
    SubEntities.Add(AEntity);
  end;

  FEntities := SubEntities;
  FAddedEntities.Clear;

end;

function TEntityManager.GetEntities: specialize TList<TEntity>;
begin
  Result := FEntities;
end;

function TEntityManager.GetEntity(ATag: string): TEntity;
var
  I: Integer;
begin
  Result := Nil;
  For I := 0 TO FEntities.Count - 1 do
  begin
    If FEntities[I].tag = ATag Then
    Begin
      Result := FEntities[I];
      break;
    end;
  end;
end;

end.
