unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil, component;

type

  TEntity = class
  private
    FPosition: TPositionComponent;
    FAnimation: TAnimationComponent;
    FIsAlive: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write FPosition;
    property animation: TAnimationComponent read FAnimation write FAnimation;
    property IsLive: boolean read FIsAlive write FIsAlive;
  end;



implementation

constructor TEntity.Create;
begin
  FIsAlive := True;
end;

destructor TEntity.Destroy;
begin
  if Assigned(FPosition) then
  begin
    LogDebug('Remove Position Component');
    FreeAndNil(FPosition);
  end;

  If Assigned(FAnimation) then
  begin
    LogDebug('Remove Animation Component');
    FreeAndNil(FAnimation);
  end;
end;


end.
