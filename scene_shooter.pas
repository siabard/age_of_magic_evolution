unit scene_shooter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, KeyInput, scene_map, tilemap, atlas, physics_util;

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
  end;

implementation

uses entity, component, Generics.Collections, game_types, animation, camera;
{ TSceneShooter }

constructor TSceneShooter.Create(AM: TAssetManager; AR: PSDL_Renderer;
  AK: TKeyInput);
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
  inherited;

  if dt = 0 then
    FPS := 0
  else
    FPS := 1000 div Trunc(dt * 1000);
  FTextBox.Text := Format('DT : %8d%sFPS: %8d', [Trunc(dt * 1000), sLineBreak, FPS]);


end;

procedure TSceneShooter.SceneRender;
begin
  SDL_RenderClear(FRenderer);

  Self.RenderSystem;

  { 기타 정보창은 모든 렌더링 이후에 진행 }
  FTextBox.DrawPanel(FRenderer);
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
  Angle: Double;
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

          If Assigned(AEntity.input) Then
          begin
            Angle := 0;
            Center.x:=32;
            Center.y:=32;
             If AEntity.input.left = true then
                Angle := Angle - 10;
             If AEntity.input.right = true then
                Angle := Angle + 10;

             SDL_RenderCopyEx(FRenderer, texture, @SrcRect, @DstRect, Angle, @Center, 0);
          end
          Else
          begin
            SDL_RenderCopy(FRenderer, texture, @SrcRect, @DstRect);
          end;

        end;
      end;
    end;
  end;
end;

end.
