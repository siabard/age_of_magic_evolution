unit asset_manager;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, sdl2, sdl2_image,
  Generics.Collections, LogUtil;

type

  TTextureDictionary = specialize TDictionary<string, PSDL_Texture>;

  TAssetManager = class
  private
    ASdlRenderer: PSDL_Renderer;
    ATextures: TTextureDictionary;
  public
    constructor Create(BRenderer: PSDL_Renderer);
    destructor Destroy; override;
    procedure AddTexture(textureId: string; PTexture: PSDL_Texture);
    procedure LoadTexture(textureId: string; Path: pansichar);
    function GetTexture(textureId: string): PSDL_Texture;
  end;


implementation

constructor TAssetManager.Create(BRenderer: PSDL_Renderer);
begin
  ASdlRenderer := BRenderer;
  ATextures := TTextureDictionary.Create;
end;

destructor TAssetManager.Destroy;
var
  TextureValue: PSDL_Texture;
begin
  for TextureValue in ATextures.Values do
  begin
    LogDebug('ASSET_MANAGER::Destroy Textuer');

    SDL_DestroyTexture(TextureValue);
  end;
  ATextures.Free;
end;

procedure TAssetManager.AddTexture(textureId: string; PTexture: PSDL_Texture);
begin
  LogDebug('AssetManager::AddTexture::' + textureId);
  if PTexture <> nil then
  begin
    ATextures.Add(textureId, PTexture);
  end
  else
  begin
    WriteLn(' Unalbe add texture: ', textureId);
  end;
end;

function TAssetManager.GetTexture(textureId: string): PSDL_Texture;
var
  SearchedValue: PSDL_Texture;
begin
  if ATextures.TryGetValue(textureId, SearchedValue) then
    Result := SearchedValue
  else
    Result := nil;
end;


procedure TAssetManager.LoadTexture(textureId: string; Path: pansichar);
var
  ATexture: PSDL_Texture;
begin
  LogDebug(' Add New Texture : ' + textureId);
  ATexture := IMG_LoadTexture(ASdlRenderer, Path);
  Self.AddTexture(textureId, ATexture);
end;

end.
