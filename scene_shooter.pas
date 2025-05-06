unit scene_shooter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, KeyInput, scene_map, tilemap,
  atlas, physics_util;

type
  { Scene Shooter }

  { TSceneShooter }

  TSceneShooter = class(TSceneMap)
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
    procedure RenderSystem;
    procedure BoostSystem(dt: real);
    procedure BorderSystem;
  end;

implementation

uses entity, component, Generics.Collections, game_types, animation, camera;
  { TSceneShooter }

constructor TSceneShooter.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
begin
  inherited;

  FCamera.MaxX := 640;
  FCamera.MaxY := 480;
end;

destructor TSceneShooter.Destroy;
begin
  inherited Destroy;
end;

procedure TSceneShooter.SceneUpdate(dt: real);
var
  FPS: integer;
begin
  FEntityManager.Update;

  if dt = 0 then
    FPS := 0
  else
    FPS := 1000 div Trunc(dt * 1000);
  FTextBox.Text := Format('DT : %8d%sFPS: %8d', [Trunc(dt * 1000), sLineBreak, FPS]);

  InputSystem;
  MovementSystem(dt);
  BoostSystem(dt);
  CameraSystem(dt);
  AnimationSystem(dt);
  CollisionSystem;
  BorderSystem;

end;

procedure TSceneShooter.SceneRender;
begin
  SDL_RenderClear(FRenderer);

  Self.RenderSystem;

  { 기타 정보창은 모든 렌더링 이후에 진행 }
  FTextBox.Draw(FRenderer);


  SDL_RenderPresent(FRenderer);
end;

procedure TSceneShooter.RenderSystem;
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
  Angle: double;
  Center: TSDL_Point;
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

          if Assigned(AEntity.input) then
          begin
            Angle := 0;
            Center.x := 32;
            Center.y := 32;
            if AEntity.input.left = True then
              Angle := Angle - 10;
            if AEntity.input.right = True then
              Angle := Angle + 10;

            SDL_RenderCopyEx(FRenderer, texture, @SrcRect, @DstRect, Angle, @Center, 0);
          end
          else
          begin
            SDL_RenderCopy(FRenderer, texture, @SrcRect, @DstRect);
          end;

        end;
      end;
    end;
  end;
end;

procedure TSceneShooter.BoostSystem(dt: real);
var
  AEntity: TEntity;
begin
  AEntity := FEntityManager.GetEntity('player');
  { Plyaer 의 Boost 수치를 내린다. }
  if Assigned(AEntity.movement) then
  begin

    if AEntity.movement.boost > 0 then
    begin
      { boost 가 남아 있는 동안은 추가적인 이동효과를 얻는다. }
      AEntity.movement.boost := AEntity.movement.boost - Trunc(dt * 1000);

      { boost 가 다 떨어지면 cooldown 을 설정한다. }
      if AEntity.movement.boost <= 0 then
        AEntity.movement.Cooldown := 6000;

      if Assigned(AEntity.position) then
      begin
        AEntity.position.X := AEntity.position.X + Round(AEntity.movement.X * dt);
        AEntity.position.Y := AEntity.position.Y + Round(AEntity.movement.Y * dt);
      end;

    end
    else
    begin
      { boost 가 꺼져있는 동안은 cooldown 을 줄인다. }
      AEntity.movement.Cooldown := AEntity.movement.Cooldown - Trunc(dt * 1000);
      if AEntity.movement.Cooldown <= 0 then
        AEntity.movement.Cooldown := 0;
    end;
  end;
end;

procedure TSceneShooter.BorderSystem;
var
  Player: TEntity;
  APosComp: TPositionComponent;
  Top, Left, Right, Bottom: integer;

  AAnimComp: TAnimationComponent;
  AAnimation: TAnimation;
  AnimRect: RRect;

  PlayerTop, PlayerLeft, PlayerBottom, PlayerRight: integer;
begin

  Top := 0;
  Bottom := 480;
  Left := 0;
  Right := 640;

  Player := EntityManager.GetEntity('player');

  if Assigned(Player.animation) and Assigned(Player.position) then
  begin
    { 플레이어 사각 크기 }
    AAnimComp := Player.animation;
    APosComp := Player.position;
    AAnimComp.Animations.TryGetValue(AAnimComp.CurrentAnimation, AAnimation);

    if AAnimation <> nil then
    begin
      // 현재 프레임에 걸린 값을 찾는다.
      AnimRect := AAnimation.GetFrames[AAnimComp.CurrentFrame];

      PlayerTop := Player.position.Y;
      PlayerBottom := Player.position.Y + AnimRect.RH;
      PlayerLeft := Player.position.X;
      PlayerRight := Player.position.X + AnimRect.RW;

      // Left Bound
      if PlayerLeft < Left then
        Player.position.X := Player.position.X + (Left - PlayerLeft);

      // Right Bound
      if PlayerRight > Right then
        Player.position.X := Player.position.X - (PlayerRight - Right);

      // Top Bound
      if PlayerTop < Top then
        Player.position.Y := Player.position.Y + (Top - PlayerTop);

      // Bottom Bound
      if PlayerBottom > Bottom then
        Player.position.Y := Player.position.Y - (PlayerBottom - Bottom);

    end;
  end;

end;

end.
