unit entity_manager;

{$mode ObjFPC}{$H+}{$modeswitch functionreferences}

interface

uses
  Classes, SysUtils, entity, Generics.Collections, Generics.Defaults, LogUtil;

type
  TListEntity = specialize TList<TEntity>;

  generic TComparisonFunc<T> = function(constref Left, Right: T): integer;


  { TEntityManager }

  TEntityManager = class
  private
    FEntities: specialize TList<TEntity>;
    FAddedEntities: specialize TList<TEntity>;
    FEntityGroups: specialize THashMap<string, specialize TList<TEntity>>;
    entity_id: integer;
    FEntityComparer: specialize IComparer<TEntity>;
  public
    constructor Create;
    destructor Destroy; override;
    function AddEntity(groupName: string = 'default'): TEntity;
    procedure Update();
    function GetEntities: specialize TList<TEntity>;
    function GetEntity(ATag: string): TEntity;
    function GetEntityGroup(groupname: string): TListEntity;
    property EntityComparer: specialize IComparer<TEntity> read FEntityComparer;
  end;


implementation

constructor TEntityManager.Create;

begin
  FEntityComparer := specialize TComparer<TEntity>.Construct(@CompareEntities);
  FEntities := specialize TList<TEntity>.Create;
  FAddedEntities := specialize TList<TEntity>.Create;
  FEntityGroups := specialize THashMap<string, TListEntity>.Create;
  entity_id := 0;
end;

destructor TEntityManager.Destroy;
var
  AListEntity: TListEntity;
  AEntity: TEntity;
  I: integer;
begin

  // 엔터티 그룹 지우기
  for AListEntity in FEntityGroups.Values do
  begin
    for I := 0 to AListEntity.Count - 1 do
    begin
      AListEntity[I].IsLive := False;
    end;
    AListEntity.Free;
  end;
  FEntityGroups.Free;

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

function TEntityManager.AddEntity(groupName: string = 'default'): TEntity;
var
  AEntity: TEntity;
  AListEntity: TListEntity;
begin
  Inc(entity_id);
  LogDebug('TEntityManager.AddEntity');
  AEntity := TEntity.Create;
  AEntity.setNid(entity_id);
  FAddedEntities.Add(AEntity);


  if not FEntityGroups.ContainsKey(groupName) then
  begin
    AListEntity := TListEntity.Create;
    FEntityGroups.Add(groupName, AListEntity);
  end
  else
    FEntityGroups.TryGetValue(groupName, AListEntity);
  AListEntity.Add(AEntity);

  Result := AEntity;
end;

procedure TEntityManager.Update;
var
  AEntity: TEntity;
  SubEntities: specialize TList<TEntity>;
begin

  // 삭제된 Entity 항목을 모두 지운다.
  SubEntities := specialize TList<TEntity>.Create;

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
  FEntities.Sort(FEntityComparer);
  FAddedEntities.Clear;

end;

function TEntityManager.GetEntities: specialize TList<TEntity>;
begin
  Result := FEntities;
end;

function TEntityManager.GetEntity(ATag: string): TEntity;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FEntities.Count - 1 do
  begin
    if FEntities[I].tag = ATag then
    begin
      Result := FEntities[I];
      break;
    end;
  end;
end;

function TEntityManager.GetEntityGroup(groupname: string): TListEntity;
var
  AListEntity: TListEntity;
begin
  Result := nil;
  if FEntityGroups.TryGetValue(groupName, AListEntity) then
    Result := AListEntity;
end;

end.
