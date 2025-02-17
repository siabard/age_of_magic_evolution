unit scene;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, entity, Generics.Collections, textbox;

type

  ESceneType = (title_scene, map_scene, battle_scene, end_scene);

  TScene = class
  private
    AAssetManager: TAssetManager;
    ASceneType: ESceneType;
    ARenderer: PSDL_Renderer;
    AEntities: specialize TList<TEntity>;
    AddedEntities: specialize TList<TEntity>;
    ATextBox: TTextBox;
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
    property Renderer: PSDL_Renderer read ARenderer write ARenderer;
    property AssetManager: TassetManager read AAssetManager write AAssetManager;

  end;

implementation

constructor TScene.Create(AM: TAssetManager; AR: PSDL_Renderer);
begin
  AAssetManager := AM;
  ARenderer := AR;
  AEntities := specialize TList<TEntity>.Create;
  AddedEntities := specialize TList<TEntity>.Create;
  ATextBox := TTextBox.Create(0, 0, 192, 32, 4, 4);

  // 텍스트박스 폰트 설정
  ATextBox.korFontTexture := AAssetManager.GetTexture('hangul');
  ATextBox.engFontTexture := AAssetManager.GetTexture('ascii');
  ATextBox.boxTexture := AAssetManager.GetTexture('panel');

  If Assigned(ATextBox.boxTexture) Then
     WriteLn(' Panel texture ');

end;

destructor TScene.Destroy;
var
  AEntity: TEntity;
begin
  ATextBox.Free;
  for AEntity in AEntities do
  begin
    AEntity.Free;
  end;
  AEntities.Free;
  inherited;
end;

procedure TScene.SceneUpdate(dt: real);
var
  AEntity: TEntity;
  SubEntities: specialize TList<TEntity>;
begin

  ATextBox.Text := Format('%4d', [Trunc(dt * 1000)]);

  // 삭제된 Entity 항목을 모두 지운다.
  SubEntities := specialize TList<TEntity>.Create;
  for AEntity in AEntities do
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
  for AEntity in AddedEntities do
  begin
    SubEntities.Add(AEntity);
  end;

  AEntities.Free;
  AEntities := SubEntities;

end;

procedure TScene.SceneRender();
var
  itemTexture: PSDL_Texture;
begin
  SDL_RenderClear(ARenderer);

  itemTexture := AAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(ARenderer, itemTexture, nil, nil);

  ATextBox.DrawPanel(ARenderer);

    { ATextBox.DrawString(ARenderer,
      'ABCDEFG ijkl 123 가각단댕. 세상은 더 이상 커질 수 없을 정도로 커진다. 텍스트의 크기도 마찬가지. 점점 커진다.');
    }
  ATextBox.Draw(ARenderer);
  SDL_RenderPresent(ARenderer);

end;

end.
