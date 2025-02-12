unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Components, Generics.Collections;

type
  TEntity = class
  private
    FComponents: specialize THashSet<TComponent>;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TEntity.Create;
begin
  FComponents := specialize THashSet<TComponent>.Create;
end;

destructor TEntity.Destroy;
var
  comp: TComponent;
begin
  for comp in FComponents do
    comp.Free;
  FComponents.Free;
  inherited Destroy;

end;

end.
