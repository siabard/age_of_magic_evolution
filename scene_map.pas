unit scene_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, textbox, scene, KeyInput, entity;

type


  { TSceneMap }

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
    procedure CameraSystem(dt: real);
    procedure DoAction(ACode: integer; AAct: EActionType); override;
    procedure InputSystem;
    procedure MovementSystem(dt: real);
    procedure CollisionSystem;
  end;

implementation

uses
  atlas,
  component,
  animation,
  camera,
  game_types,
  physics_util,
  tilemap,
  LogUtil,
  Generics.Defaults,
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
  ATeleport: TTeleportComponent;
  DefaultCollider: RRect;
begin
  inherited;

  FSceneType := map_scene;
  FTextBox := TTextBox.Create(0, 0, 192, 40, 4, 4);

  FTextBox.boxTexture := FAssetManager.GetTexture('panel');
  // 텍스트박스 폰트 설정
  FTextBox.korFontTexture := FAssetManager.GetTexture('hangul');
  FTextBox.engFontTexture := FAssetManager.GetTexture('ascii');

  {
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
  }


  { 충돌용 엔터티 (텔레포트 ) 만들어보기 }
  {
  AEntity := FEntityManager.AddEntity();
  APosComp := TPositionComponent.Create('teleport pos');
  APosComp.X := 62;
  APosComp.Y := 66;
  APosComp.PX := 62;
  APosComp.PY := 66;
  with DefaultCollider do
  begin
    RX := 0;
    RY := 0;
    RW := 16;
    RH := 16;
  end;
  ACollider := TCollideComponent.Create('teleport bound');
  ACollider.BoundBox := DefaultCollider;

  ATeleport := TTeleportComponent.Create('teleport comp');
  with ATeleport.Pos do
  begin
    RX := 12;
    RY := 12;
  end;

  AEntity.position := APosComp;
  AEntity.collide := ACollider;
  AEntity.teleporter := ATeleport;
  }


  { Action 설정 }
  {* Scene 의 Init 으로 옮겨짐

  Self.RegisterAction(SDLK_UP, move_up);
  Self.RegisterAction(SDLK_DOWN, move_down);
  Self.RegisterAction(SDLK_LEFT, move_left);
  Self.RegisterAction(SDLK_RIGHT, move_right);
  Self.RegisterAction(SDLK_SPACE, move_action);

  *}
end;

destructor TSceneMap.Destroy;
begin
  FTextBox.Free;

  inherited;
end;

procedure TSceneMap.SceneUpdate(dt: real);
var
  FPS: integer;
begin
  inherited;

  if dt = 0 then
    FPS := 0
  else
    FPS := 1000 div Trunc(dt * 1000);
  FTextBox.Text := Format('DT : %8d%sFPS: %8d', [Trunc(dt * 1000), sLineBreak, FPS]);

  InputSystem;
  MovementSystem(dt);
  CameraSystem(dt);
  AnimationSystem(dt);
  CollisionSystem;

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

  Self.RenderSystem;

  { 기타 정보창은 모든 렌더링 이후에 진행 }
  FTextBox.Draw(FRenderer);


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
  Y, X, I: integer;
  SrcRect: TSDL_Rect;
  DstRect: TSDL_Rect;
  AAnimation: TAnimation;
  AnimRect: RRect;
  texture: PSDL_Texture;
  CameraClip: RRect;
  ATileMap: RTilemap;
  ALayer: RLayer;
  gid: integer;
  ATileset: RTileset;
  TI: integer; // 타일셋 인덱스
  CI: integer; // 레이어내 셀의 인덱스
  AAtlas: TAtlas;
  AI: integer; // 해당 타일셋에서 firstgid 를 뺀 진짜 값.
  ARect: RRect; // Atlas의 좌표
begin

  {
  // Tile 출력해보기
  // 레이어의 가로와 세로는 ATilemap 에 들어있음.
  if FTileMap.TryGetValue(FMapName, ATileMap) then
  begin
    for I := 0 to ATileMap.FLayers.Count - 1 do
    begin
      for Y := 0 to ATileMap.FHeight - 1 do
      begin
        for X := 0 to ATilemap.FWidth - 1 do
        begin
          CI := Y * ATileMap.FHeight + X;
          ALayer := ATilemap.FLayers[I];
          Gid := ALayer.Data[CI];

          // Gid 가 0인 내역은 빈 곳임.
          if Gid > 0 then
          begin
            TI := getTilesetIndex(ATileMap.FTilesets, Gid);

            // 해당하는 타일셋을 얻었으니 해당하는 텍스쳐 아틀라스에서
            // Rect를 받아옴.
            // 원칙은 얘네들도 전부 Entity로 만들어야하지만, 일단 여기에서는
            // 출력 가능여부만 확인
            // 이후에 Tile 관련 Entity로 전부 바꿀 것
            ATileset := ATilemap.FTilesets[TI];
            AAtlas := AssetManager.GetAtlas(ATileset.tilesetname);
            texture := AssetManager.GetTexture(ATileset.tilesetname);
            AI := Gid - ATileset.firstgid;
            ARect := AAtlas.Rects[AI];

            // 이제 렌더링한다.
            SrcRect := RectToSdl2Rect(ARect);
            DstRect.x:= ATileset.tilewidth * x;
            DstRect.y := ATileset.tileheight * y;
            DstRect.w := ATileset.tilewidth;
            DstRect.h := ATileset.tileheight;
            SDL_RenderCopy(FRenderer, texture, @SrcRect, @DstRect);
          end;

        end;
      end;
    end;
  end;

  }


  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];
    if Assigned(AEntity.animation) and Assigned(AEntity.position) then
    begin

      {
      if Assigned(Aentity.depth) then
        WriteLn('Depth : ', AEntity.tag, ' -- ', AEntity.depth.depth)
      else
        WriteLn('No Depth ', AEntity.tag);
      }
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

        CameraClip := CameraClippedRect(Sdl2RectToRect(DstRect), FCamera.GetRect);


        if (CameraClip.RW > 0) and (CameraClip.RH > 0) then
        begin

        {
        WriteLn(Format('en: %3d %3d %3d %3d', [ DstRect.x, DstRect.y, DstRect.w, DstRect.h]));
        WriteLn(Format('ca: %3d %3d %3d %3d', [FCamera.GetRect.RX, FCamera.GetRect.RY, FCamera.GetRect.RW, FCamera.GetRect.RH]));
        WriteLn(Format('CL: %3d %3d %3d %3d', [CameraClip.RX, CameraClip.RY, CameraClip.RW, CameraClip.RH]));
        }

          if DstRect.X >= FCamera.GetRect.RX then
          begin
            SrcRect.x := AnimRect.RX;
            DstRect.x := (APosComp.x - FCamera.GetRect.RX);
          end
          else
          begin
            SrcRect.x := AnimRect.RX + (AnimRect.RW - CameraClip.RW);
            DstRect.x := APosComp.x + (AnimRect.RW - CameraClip.RW) - FCamera.GetRect.RX;
          end;

          if DstRect.Y >= FCamera.GetRect.RY then
          begin
            SrcRect.Y := AnimRect.RY;
            DstRect.y := (APosComp.y - FCamera.GetRect.RY);
          end
          else
          begin
            SrcRect.Y := AnimRect.RY + (AnimRect.RH - CameraClip.RH);
            DstRect.y := APosComp.y + (AnimRect.RH - CameraClip.RH) - FCamera.GetRect.RY;
          end;


          SrcRect.w := CameraClip.RW;
          SrcRect.h := CameraClip.RH;



          DstRect.w := CameraClip.RW;
          DstRect.h := CameraClip.RH;
          // 텍스쳐
          texture := FAssetManager.GetTexture(AAnimation.TextureName);

          {
          WriteLn(Format('SRC : %d %d %d %d',
            [SrcRect.x, SrcRect.y, SrcRect.w, SrcRect.h]));
          WriteLn(
            Format('DST : %d %d %d %d', [DstRect.x, DstRect.y, DstRect.w, DstRect.h]));
          }
          SDL_RenderCopy(FRenderer, texture, @SrcRect, @DstRect);

        end;
      end;

    end;
  end;
end;

procedure TSceneMap.CameraSystem(dt: real);
var
  player: TEntity;
  PosX, PosY: integer;
  HDir: TDirection;
  VDir: TDirection;
begin

  player := FEntityManager.GetEntity('player');

  PosX := 0;
  PosY := 0;
  VDIR := dirNone;
  HDir := dirNone;

  if Assigned(player.position) then
  begin
    PosX := player.position.X;
    PosY := player.position.Y;
  end;


  if Assigned(player.input) then
  begin
    if player.input.down = True then
      VDIR := dirDown
    else if player.input.up = True then
      VDIR := dirUp
    else
      VDIR := dirNone;

    if player.input.left = True then
      HDir := dirLeft
    else if player.input.right = True then
      HDir := dirRight
    else
      HDir := dirNone;
  end;


  FCamera.Follow(PosX, PosY, VDir, HDir);
  FCamera.Update(dt);

end;

procedure TSceneMap.DoAction(ACode: integer; AAct: EActionType);
var
  tmpAct: EActionName;
  AEntities: specialize TList<TEntity>;
  AEntity: TEntity;
  I: integer;
  CurrentMap: RTilemap;
begin
  AEntities := FEntityManager.GetEntities;

  if FActionMap.TryGetValue(ACode, tmpAct) then
  begin
    if AAct = EActionType.action_start then
    begin
      for I := 0 to AEntities.Count - 1 do
      begin
        AEntity := AEntities[I];
        if AEntity.input <> nil then
          case tmpAct of
            move_down: AEntity.input.down := True;
            move_up: AEntity.input.up := True;
            move_left: AEntity.input.left := True;
            move_right: AEntity.input.right := True;
            action_boost: begin
              if Assigned(AEntity.movement) then
              begin
                if (AEntity.movement.Boost <= 0) and
                  (AEntity.movement.Cooldown <= 0) then
                begin
                  AEntity.movement.Boost := 1500;
                end;
              end;
            end;
          end;

      end;
    end
    else if AAct = EActionType.action_stop then
    begin
      for I := 0 to AEntities.Count - 1 do
      begin
        AEntity := AEntities[I];
        if AEntity.input <> nil then
          case tmpAct of
            move_down: AEntity.input.down := False;
            move_up: AEntity.input.up := False;
            move_left: AEntity.input.left := False;
            move_right: AEntity.input.right := False;
            move_action: begin
              // 30, 30 위치로 옮긴다.
              AEntity := EntityManager.GetEntity('player');

              FTileMap.TryGetValue(FMapName, CurrentMap);
              AEntity.Teleport(CurrentMap, 30, 30);

              FCamera.Teleport(CurrentMap, 30, 30);

            end;
          end;

      end;
    end;

  end;

end;

procedure TSceneMap.InputSystem;
var
  I: integer;
  AEntity: TEntity;
  AEntities: specialize TList<TEntity>;
begin
  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];
    if (AEntity.movement <> nil) and (AEntity.input <> nil) then
    begin
      AEntity.movement.X := 0;
      AEntity.movement.Y := 0;
      if AEntity.input.up then
        AEntity.movement.Y := AEntity.movement.Y - 100;
      if AEntity.input.down then
        AEntity.movement.Y := AEntity.movement.Y + 100;
      if AEntity.input.left then
        AEntity.movement.X := AEntity.movement.X - 100;
      if AEntity.input.right then
        AEntity.movement.X := AEntity.movement.X + 100;
    end;
  end;

end;

procedure TSceneMap.MovementSystem(dt: real);
var
  I: integer;
  AEntity: TEntity;
  AEntities: specialize TList<TEntity>;
begin
  AEntities := FEntityManager.GetEntities;

  for I := 0 to AEntities.Count - 1 do
  begin
    AEntity := AEntities[I];
    if (AEntity.position <> nil) and (AEntity.movement <> nil) then
    begin
      AEntity.position.PX := AEntity.position.X;
      AEntity.position.PY := AEntity.position.Y;

      AEntity.position.X := AEntity.position.X + Round(AEntity.movement.X * dt);
      AEntity.position.Y := AEntity.position.Y + Round(AEntity.movement.Y * dt);
    end;
  end;

end;

procedure TSceneMap.CollisionSystem;
var
  Collider: TEntity;
  Player: TEntity;
  Position: TPositionComponent;
  I: integer;
  J: integer;
  Entities: specialize TList<TEntity>;
  CollDir: EDirection;
  CollAmount: RVec2;
  CurrentMap: RTilemap;
begin

  Entities := FEntityManager.GetEntities;
  for  I := 0 to Entities.Count - 1 do
  begin
    Player := Entities[I];

    { 충돌, 위치 컴포넌트가 없는 경우는 충돌 체크하지 않는다.}
    if (Player.collide = nil) or (player.position = nil) then
      continue;
    for J := 0 to Entities.Count - 1 do

      if I <> J then
      begin
        Collider := Entities[J];
        if (Collider.collide = nil) or (Collider.position = nil) then
          continue;
        if Player.id <> Collider.id then
        begin
          CollDir := CollideDirection(Player, Collider);
          CollAmount := OverlapAmount(Player.GetBoundigRect, Collider.GetBoundigRect);

          { 두 물체가 충돌했을 때 이동 컴포넌트의 상황에 맞추어 위치를 결정한다. }
          if (Player.movement <> nil) and ((Player.movement.X <> 0) or
            (Player.movement.Y <> 0)) then
            Position := Player.position
          else if (Collider.movement <> nil) and
            ((Collider.movement.X <> 0) or (Collider.movement.Y <> 0)) then
            Position := Collider.position;

          if (CollAmount.Rx > 0) and (CollAmount.RY > 0) then
          begin

            {
            CollisionSystem 에서는 대상 Entity에 대해 다양한 행동을 한다.

            }
            if Assigned(Collider.teleporter) then
            begin
              { Teleport 를 만나면 순간이동 }
              if FTileMap.TryGetValue(FMapName, CurrentMap) then
              begin
                Player.Teleport(CurrentMap, Collider.teleporter.Pos.RX,
                  Collider.teleporter.Pos.RY);
                FCamera.Teleport(CurrentMap, Collider.teleporter.Pos.RX,
                  Collider.teleporter.Pos.RY);
              end;
            end
            else
            { 지금은 디폴트가 이동방지이지만, 나중에는
              특정한 컴포넌트가 있을 때에만 동작해야한다.
            }
            begin
              case CollDir of
                EDirection.dir_up: Position.Y := Position.Y + CollAmount.RY;
                EDirection.dir_down: Position.Y := Position.Y - CollAmount.RY;
                EDirection.dir_left: Position.X := Position.X + CollAmount.RX;
                EDirection.dir_right: Position.X := Position.X - CollAmount.RX;
              end;
            end;

          end;

        end;
      end;

  end;
end;

end.
