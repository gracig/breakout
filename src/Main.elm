module Main exposing (main)

import Playground as P exposing (..)
import Math.Vector2 as V exposing (..)
import Set exposing (Set)
import Dict exposing (Dict)

main = P.game view update reset

-- Memory
type alias Memory = 
  { player : Player
  , balls : List Ball
  , bricks : List Brick
  , paused : Bool
  }
type alias Coord =
  { x : Float
  ,  y : Float
  }
type alias Player = 
  { life : Float
  , score: Float
  , coord: Coord
  , level: Int
  , width: Float
  , height: Float
  , speedRatio: Float
  , widthRatio: Float
  , heightRatio: Float
  }
type alias Ball = 
  { speed: V.Vec2  
  , coord: Coord
  , speedRatio: Float
  , radius: Float
  , glued: Bool
  , dropped: Bool
  , direction: V.Vec2
  , brickHits: List Int -- the brick index
  }
type alias Brick = 
  { coord: Coord
  , width: Float
  , height: Float
  , life: Float
  , position: ( Int, Int)
  , widthRatio : Float
  , heightRatio : Float

  }
reset : Memory
reset = 
  { paused = False
  , player = resetPlayer
  , balls = resetBalls
  , bricks = resetBricks
  }

resetPlayer : Player
resetPlayer = 
  { coord = { x = 0, y = 0}
  , score = 0
  , life = 3
  , level = 1
  , width = 1
  , height = 1
  , speedRatio = 0.01
  , widthRatio = 0.1
  , heightRatio = 0.025
  }

resetBalls : List Ball
resetBalls = 
  [ { coord = { x = 0, y = 0}, direction = V.vec2 0.25 1 ,speedRatio = 1, speed = V.vec2 10 10, radius = 10, glued = True, dropped = False, brickHits = [] }]

resetBricks : List Brick
resetBricks = 
  [ { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-3,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-3,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-3,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-3,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-3,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-3,0), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-2,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-2,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-2,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-2,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-2,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-2,0), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-1,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-1,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-1,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-1,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-1,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-1,0), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-0,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-0,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-0,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-0,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(-0,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 10, width=10, height=10, position=(-0,0), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(1,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(1,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(1,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(1,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(1,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(1,0), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(2,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(2,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(2,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(2,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(2,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(2,0), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(3,5), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(3,4), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(3,3), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(3,2), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(3,1), widthRatio=0.075, heightRatio=0.05} 
  , { coord = { x = 0, y = 0}, life = 1, width=10, height=10, position=(3,0), widthRatio=0.075, heightRatio=0.05} 
  ]


view : P.Computer -> Memory -> List P.Shape
view computer memory = 
    viewPlayer memory.player 
    :: viewBalls memory.balls 
    ++ viewExtra computer memory
    ++ viewGameOver computer memory
    ++ viewBricks memory memory.bricks

viewExtra : P.Computer -> Memory -> List P.Shape
viewExtra computer memory = 
  let 
    scale = computer.screen.height * 0.002
    shape = 
      words black 
       (  "Life  : " ++ (String.fromFloat memory.player.life) 
       ++ " | Score : " ++ (String.fromFloat memory.player.score)
       )
      
  in
    [ shape |> P.moveY (computer.screen.top - (scale * 15))]

viewGameOver : P.Computer -> Memory -> List P.Shape
viewGameOver computer memory = 
  if memory.player.life == 0 then
    [ words black "Game Over" |> P.scale (computer.screen.height*0.01)
    , words black "Press <Enter> to restart" |> P.scale (computer.screen.height*0.0025) |> P.moveY (computer.screen.height * 0.1 * -1) ]
  else 
    []
viewBalls : List Ball -> List P.Shape
viewBalls balls = List.map viewBall balls

viewBricks : Memory -> List Brick -> List P.Shape
viewBricks memory bricks = 
  if memory.player.life > 0 then
    List.map viewBrick bricks
  else []

viewBrick : Brick -> P.Shape
viewBrick brick = 
  P.rectangle P.blue brick.width brick.height |> P.move brick.coord.x brick.coord.y

viewBall : Ball -> P.Shape
viewBall ball = 
  P.circle P.black ball.radius |> P.move ball.coord.x ball.coord.y

viewPlayer : Player -> P.Shape
viewPlayer player = 
  P.rectangle P.black player.width player.height |> P.move player.coord.x player.coord.y

update : P.Computer -> Memory -> Memory
update computer memory = 
  if memory.paused then
    memory
      |> checkPause computer
  else
    memory
      |> checkPause computer
      |> resetOnEnter computer
      |> unglueBalls computer
      |> removeDroppedBalls
      |> loseLifeOnZeroBalls
      |> updatePlayer computer
      |> updateBricks computer
      |> updateBalls computer
      |> updateBrickHits
      |> resetBricksIfEmpty

updateBricks: P.Computer -> Memory -> Memory
updateBricks computer memory = 
  { memory | bricks = List.map ( 
      translateBrick computer
      )  memory.bricks
  }

resetBricksIfEmpty: Memory -> Memory
resetBricksIfEmpty memory =
  if List.isEmpty memory.bricks then
    { memory | bricks = resetBricks }
  else memory

translateBrick: P.Computer -> Brick -> Brick
translateBrick computer brick = 
    let width = computer.screen.width * brick.widthRatio
        height = computer.screen.height * brick.heightRatio
        pw = computer.screen.width * 0.02
        ph = 0
        coordX = toFloat(Tuple.first (brick.position)) * (width + pw)
        coordY = toFloat(Tuple.second (brick.position)) * (height + ph)
    in
        { brick | width = width, height = height, coord = coord coordX coordY }

getBrickAtIndex: Int -> List Brick -> Maybe Brick
getBrickAtIndex idx bricks = 
  List.head  (List.drop idx bricks)
delBrickAtIndex: Int -> List Brick -> List Brick
delBrickAtIndex idx bricks = 
  (List.take idx bricks) ++ (List.drop (idx+1) bricks)

updateBrickHits: Memory -> Memory
updateBrickHits memory = 
  let 
    processBrickHit: Maybe Brick -> List Brick -> List Brick
    processBrickHit mb bricks = 
      case mb of
         Nothing -> bricks
         Just brick ->
          if brick.life - 1 == 0 then
            bricks --brick remains deleted
          else
            { brick | life = brick.life - 1 } :: bricks --reinsert break with less life
    increaseScore : Player -> Player
    increaseScore player =
      { player | score = player.score + 1000}
  in
    case List.foldl 
      ( \b1 (m1,bs) ->
        (  List.foldl 
             ( \idx m2 -> 
               {m2 | bricks = processBrickHit (getBrickAtIndex idx m2.bricks) (delBrickAtIndex idx m2.bricks)
               , player = increaseScore m2.player}
              ) m1 b1.brickHits
        , {b1 | brickHits = []} :: bs
        )
      ) (memory,[]) memory.balls  of
      (mem, balls) -> { mem | balls = balls}

checkPause: P.Computer -> Memory -> Memory
checkPause computer memory = 
  if Set.member "p" computer.keyboard.keys || Set.member "P" computer.keyboard.keys then
    { memory | paused = True }
  else  if computer.keyboard.space then
    { memory | paused = False }
  else
    memory
resetOnEnter: P.Computer -> Memory -> Memory
resetOnEnter computer memory = 
  if computer.keyboard.enter && memory.player.life == 0 then
    reset
  else
    memory

unglueBalls: P.Computer -> Memory -> Memory
unglueBalls computer memory = 
  { memory | balls = List.foldl ( 
    \ball xs -> 
      if computer.keyboard.space && ball.glued then 
        {ball | glued = False } :: xs
      else ball::xs 
    ) [] memory.balls 
  }

loseLife : Player -> Player
loseLife player = 
    { player | life = if player.life > 0 then player.life - 1 else 0 }
resetMemoryBalls: Memory -> Memory
resetMemoryBalls memory = 
  if memory.player.life> 0 then
    { memory | balls = resetBalls}
  else
    memory

loseLifeOnZeroBalls: Memory -> Memory
loseLifeOnZeroBalls memory = 
  if List.isEmpty memory.balls then
    { memory | player = loseLife memory.player}
    |> resetMemoryBalls
  else 
    memory
removeDroppedBalls: Memory -> Memory
removeDroppedBalls memory = 
  { memory | balls = List.foldl (\ball xs -> if ball.dropped then xs else ball::xs ) [] memory.balls }

updateBalls: P.Computer -> Memory -> Memory
updateBalls computer memory = 
  { memory | balls = memory.balls 
                |> resizeBalls computer memory
                |> moveBalls computer memory
  }


resizeBalls: P.Computer -> Memory -> List Ball -> List Ball
resizeBalls computer memory balls = List.map (resizeBall computer memory ) balls

resizeBall: P.Computer -> Memory -> Ball -> Ball
resizeBall computer memory ball = 
  let
    speedx = ball.speedRatio * (computer.screen.width * 0.01)
    speedy = ball.speedRatio * (computer.screen.height * 0.01)
  in
    { ball | radius = memory.player.height /2, speed= V.vec2 speedx speedy }
  

moveBalls: P.Computer -> Memory -> List Ball -> List Ball
moveBalls computer memory balls = List.map (moveBall computer memory) balls

calcNextCoord: Coord -> V.Vec2 -> V.Vec2 -> Coord
calcNextCoord actual direction speed =  
  coord 
    ((V.getX speed ) * (V.getX direction ) + actual.x)
    ((V.getY speed ) * (V.getY direction )  + actual.y)

moveBall: P.Computer -> Memory -> Ball -> Ball
moveBall computer memory ball = 
  if ball.glued then
    { ball | coord = coord memory.player.coord.x (memory.player.coord.y + memory.player.height)} 
  else
    { ball | coord = calcNextCoord ball.coord ball.speed ball.direction 
    } |> checkBallHits computer memory

changeBallDirY : Ball -> Ball
changeBallDirY ball =
  { ball | direction =  V.vec2 (V.getX ball.direction) (V.getY ball.direction * -1) }
changeBallDirX : Ball-> Ball
changeBallDirX ball =
  { ball | direction =  V.vec2 (V.getX ball.direction * -1) (V.getY ball.direction) }


changeBallDirAfterBrickHit : Brick -> Ball -> Ball
changeBallDirAfterBrickHit brick ball= 
  let 
    pright = brick.coord.x + brick.width/2 
    pleft = brick.coord.x - brick.width/2 
  in
    if ball.coord.x <= pleft then
      changeBallDirX ball
    else if ball.coord.x >=  pright then
      changeBallDirX ball
    else
      changeBallDirY ball
    

changeBallDirAfterPlayerHit : Memory -> Ball -> Ball
changeBallDirAfterPlayerHit memory ball= 
  let 
    pright = memory.player.coord.x + memory.player.width/2 
    pleft = memory.player.coord.x - memory.player.width/2 
    ballx = 
      if ball.coord.x >= pright then 
        pright - 1 
      else if ball.coord.x <= pleft then
        pleft + 1
      else 
        ball.coord.x
    pratio = memory.player.width / (pright - ballx)
    angle = 180 / pratio
    direction = V.vec2 (cos(degrees angle)) (sin(degrees angle))
  in
    { ball |  direction = direction }

dropBall : Ball -> Ball
dropBall ball =
  { ball | dropped = True}

checkBallHits : P.Computer -> Memory -> Ball -> Ball
checkBallHits computer memory ball =
  let 
    hitWallOnTop = near ball.coord.y (ball.radius/2 + (V.getY ball.speed)) computer.screen.top
    hitWallOnBottom = near ball.coord.y (ball.radius/2 + (V.getY ball.speed)) computer.screen.bottom
    hitWallOnLeft = near ball.coord.x (ball.radius/2 + (V.getX ball.speed)) computer.screen.left
    hitWallOnRight = near ball.coord.x (ball.radius/2 + (V.getX ball.speed)) computer.screen.right
    hitPlayer = 
--      near ball.coord.y ( memory.player.height + (ball.radius/2) ) memory.player.coord.y && 
--      near ball.coord.x (memory.player.width/2 + (ball.radius/2)) memory.player.coord.x
      near ball.coord.y (memory.player.height/2  + (V.getY ball.speed)) memory.player.coord.y && 
      near ball.coord.x (memory.player.width/2 + (V.getX ball.speed)) memory.player.coord.x

    hitBrick: Brick -> Ball -> Bool
    hitBrick brick b = 
      near b.coord.y ( brick.height/2 +  (b.radius/2) ) brick.coord.y && 
      near b.coord.x ( brick.width/2 + (b.radius/2)) brick.coord.x

    hitBricks: List Brick -> Ball -> Int -> Ball
    hitBricks bricks ball_ idx = 
      case bricks of
        [] -> ball_
        brick::xs ->
          if hitBrick brick ball_ then
            {ball_ | brickHits = idx :: ball_.brickHits } |> changeBallDirAfterBrickHit brick
          else
            hitBricks xs ball_ (idx+1)
    
  in    
    if hitWallOnTop then
      ball |> changeBallDirY 
    else if hitWallOnLeft then
      ball |> changeBallDirX
    else if hitWallOnRight then
      ball |> changeBallDirX
    else if hitWallOnBottom then
      ball |> changeBallDirY |> dropBall
    else if hitPlayer then
      ball |> changeBallDirAfterPlayerHit memory
    else 
      hitBricks memory.bricks ball 0


updatePlayer: P.Computer -> Memory -> Memory
updatePlayer computer memory = 
  { memory | player = memory.player 
                |> resizePlayer computer
                |> movePlayer computer
                |> checkPlayerHits computer  
  }

resizePlayer: P.Computer -> Player -> Player
resizePlayer computer player = 
    let 
      width = computer.screen.width * player.widthRatio
      height = computer.screen.height * player.heightRatio
    in
        { player | width = width, height = height }

playerSpeed: Computer -> Player -> Float
playerSpeed computer player = computer.screen.width * player.speedRatio


movePlayer: P.Computer -> Player -> Player
movePlayer computer player =
  let
    speedFactor = playerSpeed computer player
    moveLeft = coord (player.coord.x + speedFactor * -1) (computer.screen.bottom + 25)
    moveRight = coord (player.coord.x + speedFactor) (computer.screen.bottom + 25)
    standStill = coord (player.coord.x) (computer.screen.bottom + 25)
    -- { player | coord = moveCoord player.coord {x=computer.mouse.x,y=computer.screen.bottom + 25}}
  in
    if computer.keyboard.left then
      { player | coord =  moveLeft }
    else if computer.keyboard.right then
      { player | coord = moveRight}
    else
      { player | coord = standStill }

checkPlayerHits : P.Computer -> Player -> Player
checkPlayerHits computer player =
  let 
    speedFactor = playerSpeed computer player
    pright = player.coord.x + player.width/2 
    pleft = player.coord.x - player.width/2 
    hitWallOnLeft = near pleft speedFactor computer.screen.left
    hitWallOnRight = near pright speedFactor computer.screen.right
  in    
    if hitWallOnLeft then
      { player | coord = {x = computer.screen.left + player.width/2 + 1, y= player.coord.y}}
    else if hitWallOnRight then
      { player | coord = {x = computer.screen.right - player.width/2 - 1, y= player.coord.y}}
    else player


coord: Float -> Float -> Coord
coord x y = { x = x, y = y }

-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m =
    m >= n-c && m <= n+c

