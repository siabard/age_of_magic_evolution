unit scene_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, textbox, scene, KeyInput, entity;

type


  TSceneMap = class(TScene)
  protected
    FTextBox: TTextBox;
    FPlayer: TEntity;
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
    procedure AnimationSystem(dt: real);
    procedure RenderSystem;
    procedure DoAction(ACode: integer; AAct: EActionType); override;
    procedure InputSystem;
    procedure MovementSystem(dt: real);
    procedure CollisionSystem;
  end;

implementation

uses
  component,
  animation,
  game_types,
  physics_util,
  Generics.Collections;

constructor TSceneMap.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
var
  AEntity: TEntity;
  AAnimation: TAnimation;
  AAnimComp: TAnimationComponent;
  APosComp: TPositionComponent;
  AInput: TInputComponent;
  AMovement: TMovementComponent;
  ACollider: TCollideComponent;
  DefaultCollider: RRect;
begin
  inherited;

  FSceneType := map_scene;
  FTextBox := TTextBox.Create(0, 0, 192, 32, 4, 4);

  FTextBox.boxTexture := FAssetManager.GetTexture('panel');
  // 텍스트박스 폰트 설정
  FTextBox.korFontTexture := FAssetManager.GetTexture('hangul');
  FTextBox.engFontTexture := FAssetManager.GetTexture('ascii');

  // 신규 엔터티 생성해보기 (Player)
  AEntity := FEntityManager.AddEntity();
  FPlayer := AEntity;
  // animation sword 가짐
  AAnimation := FAssetManager.GetAnimation('sword');
  AAnimComp := TAnimationComponent.Create('sword anim');
  AAnimComp.SetAnimation('sword', AAnimation);
  AAnimComp.CurrentAnimation := 'sword';
  AAnimComp.Duration := 300;
  APosComp := TPositionComponent.Create('sword pos');
  APosComp.X := 120;
  APosComp.Y := 150;
  APosComp.PX := 120;
  APosComp.PY := 150;

  AInput := TInputComponent.Create('player input');

  AMovement := TMovementComponent.Create('player move');
  AMovement.X := 0;
  AMovement.Y := 0;


  ACollider := TCollideComponent.Create('sword bound');
  with DefaultCollider do
  begin
    RX := 0;
    RY := 0;
    RW := 16;
    RH := 16;
  end;


  ACollider.BoundBox := DefaultCollider;

  AEntity.input := AInput;

  AEntity.movement := AMovement;
  AEntity.position := APosComp;
  AEntity.animation := AAnimComp;
  AEntity.collide := ACollider;

  // 신규 엔터티 생성해보기 (Collider)
  AEntity := FEntityManager.AddEntity();
  AAnimation := FAssetManager.GetAnimation('armor');
  AAnimComp := TAnimationComponent.Create('armor anim');
  AAnimComp.SetAnimation('armor', AAnimation);
  AAnimComp.CurrentAnimation := 'armor';
  AAnimComp.Duration := 300;
  APosComp := TPositionComponent.Create('Armor pos');
  APosComp.X := 240;
  APosComp.Y := 350;
  APosComp.PX := 240;
  APosComp.PY := 350;

  ACollider := TCollideComponent.Create('sword bound');
  ACollider.BoundBox := DefaultCollider;

  AEntity.animation := AAnimComp;
  AEntity.position := APosComp;
  AEntity.collide := ACollider;

  { Action 설정 }
  Self.RegisterAction(SDLK_UP, move_up);
  Self.RegisterAction(SDLK_DOWN, move_down);
  Self.RegisterAction(SDLK_LEFT, move_left);
  Self.RegisterAction(SDLK_RIGHT, move_right);
end;

destructor TSceneMap.Destroy;
begin
  FTextBox.Free;

  inherited;
end;

procedure TSceneMap.SceneUpdate(dt: real);
begin

  inherited;
  FTextBox.Text := Format('%4d', [Trunc(dt * 1000)]);
  InputSystem;
  MovementSystem(dt);
  AnimationSystem(dt);
  //CollisionSystem;

end;

procedure TSceneMap.SceneRender();
var
  itemTexture: PSDL_Texture;
begin
  SDL_RenderClear(FRenderer);

  {
  itemTexture := FAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(FRenderer, itemTexture, nil, nil);
  }
  FTextBox.DrawPanel(FRenderer);

  FTextBox.Draw(FRenderer);

  Self.RenderSystem;
  SDL_RenderPresent(FRenderer);

end;


procedure TSceneMap.AnimationSystem(dt: real);
var
  AEntity: TEntity;
  AEntities: specialize TList<TEntity>;
  AAnimationComponent: TAnimationComponent;
  I: integer;
begin
  { Entities 에서 animation 이 존재하는 항목만 검색 }
  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];
    if AEntity.animation <> nil then
    begin
      { Animation 에 대한 처리 }
      { Animation 에 대한 프레임 변경 }
      AAnimationComponent := AEntity.animation;
      AAnimationComponent.CurrentTick :=
        AAnimationComponent.CurrentTick + Round(dt * 1000);

      if AAnimationComponent.CurrentTick >= AAnimationComponent.Duration then
      begin
        AAnimationComponent.CurrentTick :=
          AAnimationComponent.CurrentTick - AAnimationComponent.Duration;
        AAnimationComponent.CurrentFrame := AAnimationComponent.CurrentFrame + 1;
        if AAnimationComponent.CurrentFrame > AAnimationComponent.CurrentMaxFrame then
          AAnimationComponent.CurrentFrame := 0;
      end;
    end;
  end;

end;

procedure TSceneMap.RenderSystem;
var
  AEntity: TEntity;
  AEntities: specialize TList<TEntity>;
  AAnimComp: TAnimationComponent;
  APosComp: TPositionComponent;
  I: integer;
  SrcRect: TSDL_Rect;
  DstRect: TSDL_Rect;
  AAnimation: TAnimation;
  AnimRect: RRect;
  texture: PSDL_Texture;
begin
  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];

    if Assigned(AEntity.animation) and Assigned(AEntity.position) then
    begin
      // 출력할 위치
      // 현재 애니메이션의 Rect 값
      // 애니메이션의 텍스쳐 값
      AAnimComp := AEntity.animation;
      APosComp := AEntity.position;
      AAnimComp.Animations.TryGetValue(AAnimComp.CurrentAnimation, AAnimation);
      if AAnimation <> nil then
      begin
        // 현재 프레임에 걸린 값을 찾는다.
        AnimRect := AAnimation.GetFrames[AAnimComp.CurrentFrame];
        SrcRect.x := AnimRect.RX;
        SrcRect.y := AnimRect.RY;
        SrcRect.w := AnimRect.RW;
        SrcRect.h := AnimRect.RH;

        DstRect.x := APosComp.X;
        DstRect.y := APosComp.Y;
        DstRect.w := AnimRect.RW;
        DstRect.h := AnimRect.RH;

        // 텍스쳐
        texture := FAssetManager.GetTexture(AAnimation.TextureName);

        SDL_RenderCopy(FRenderer, texture, @SrcRect, @DstRect);
      end;

    end;
  end;
end;

procedure TSceneMap.DoAction(ACode: integer; AAct: EActionType);
var
  tmpAct: EActionName;
begin
  if FActionMap.TryGetValue(ACode, tmpAct) then
  begin
    if AAct = EActionType.action_start then
    begin
      case tmpAct of
        move_down: FPlayer.input.down := True;
        move_up: FPlayer.input.up := True;
        move_left: FPlayer.input.left := True;
        move_right: FPlayer.input.right := True;
      end;
    end
    else if AAct = EActionType.action_stop then
    begin

      case tmpAct of
        move_down: FPlayer.input.down := False;
        move_up: Fplayer.input.up := False;
        move_left: FPlayer.input.left := False;
        move_right: FPlayer.input.right := False;
      end;
    end;

  end;

end;

procedure TSceneMap.InputSystem;
begin
  FPlayer.movement.X := 0;
  FPlayer.movement.Y := 0;
  if FPlayer.input.up then
    FPlayer.movement.Y := FPlayer.movement.Y - 100;
  if FPlayer.input.down then
    FPlayer.movement.Y := FPlayer.movement.Y + 100;
  if Fplayer.input.left then
    FPlayer.movement.X := FPlayer.movement.X - 100;
  if FPlayer.input.right then
    FPlayer.movement.X := FPlayer.movement.X + 100;

end;

procedure TSceneMap.MovementSystem(dt: real);
begin
  FPlayer.position.PX := FPlayer.position.X;
  FPlayer.position.PY := FPlayer.position.Y;

  FPlayer.position.X := FPlayer.position.X + Round(FPlayer.movement.X * dt);
  FPlayer.position.Y := FPlayer.position.Y + Round(FPlayer.movement.Y * dt);
end;

procedure TSceneMap.CollisionSystem;
var
  Collider: TEntity;
  Position: TPositionComponent;
  I: integer;
  Entities: specialize TList<TEntity>;
  CollDir: EDirection;
  CollAmount: RVec2;
begin
  Position := FPlayer.position;
  Entities := FEntityManager.GetEntities;

  for  I := 0 to Entities.Count do
  begin
    Collider := Entities[I];
    if FPlayer.id <> Collider.id then
    begin
      if Assigned(Collider.collide) and Assigned(collider.position) then
      begin
        Position := FPlayer.position;

        CollDir := CollideDirection(FPlayer, Collider);
        CollAmount := OverlapAmount(FPlayer.GetBoundigRect, Collider.GetBoundigRect);

        if (CollAmount.Rx > 0) and (CollAmount.RY > 0) then
        begin
          case CollDir of
            EDirection.dir_up: FPlayer.position.Y := FPlayer.position.Y + CollAmount.RY;
            EDirection.dir_down: FPlayer.position.Y := FPlayer.position.Y - CollAmount.RY;
            EDirection.dir_left: FPlayer.position.X := FPlayer.position.X + CollAmount.RX;
            EDirection.dir_right: FPlayer.position.X := FPlayer.position.X - CollAmount.RX;
          end;
        end;
      end;
    end;
  end;
end;

end.
