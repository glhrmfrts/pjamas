program unko;


uses sdl2, wetween;


var

  W: PSDL_Window;
  I: longint;

begin

  SDL_Init(SDL_INIT_VIDEO);
  W := SDL_CreateWindow('Hello World', 200, 200, 800, 600, SDL_WINDOW_SHOWN);

  SDL_Delay(3000);

  SDL_Quit;

end.