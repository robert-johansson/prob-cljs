const Matter = require('matter-js');

const worldWidth = 350;
const worldHeight = 500;

// Convert ProbMods world format to Matter.js bodies
// ProbMods format: [[shape, static?, [w, h]], [x, y]]
// For circles, dims is [radius] (single element); for rects, dims is [width, height]
function createBodies(world) {
  return world.map(function(obj) {
    var desc = obj[0];
    var pos = obj[1];
    var shape = desc[0];
    var isStatic = desc[1];
    var dims = desc[2];
    var x = pos[0];
    var y = pos[1];

    if (shape === 'circle') {
      var r = dims[0]; // dims[0] is the radius
      return Matter.Bodies.circle(x, y, r, { isStatic: isStatic });
    } else {
      var w = dims[0];
      var h = dims[1];
      return Matter.Bodies.rectangle(x, y, w, h, { isStatic: isStatic });
    }
  });
}

// Extract ProbMods format from Matter.js bodies, preserving original shape info
function extractWorld(bodies, originalWorld) {
  return bodies.map(function(body, i) {
    var orig = originalWorld[i];
    var desc = orig[0];
    return [
      [desc[0], desc[1], [desc[2][0], desc[2][1]]],
      [body.position.x, body.position.y]
    ];
  });
}

// Synchronous physics run: steps the engine and returns final world state
function runPhysics(steps, world) {
  var engine = Matter.Engine.create();
  // Add floor
  var bodies = createBodies(world);
  Matter.Composite.add(engine.world, bodies);

  var dt = 1000 / 60;
  var numSteps = Math.ceil(steps / (dt / 1000 * 60));
  for (var i = 0; i < numSteps; i++) {
    Matter.Engine.update(engine, dt);
  }

  return extractWorld(bodies, world);
}

// Animated physics: creates a canvas and renders with Matter.Render
function animatePhysics(steps, world) {
  var engine = Matter.Engine.create();
  var bodies = createBodies(world);
  Matter.Composite.add(engine.world, bodies);

  var canvas = document.createElement('canvas');
  canvas.width = worldWidth;
  canvas.height = worldHeight;
  canvas.style.background = '#333';
  canvas.style.borderRadius = '4px';
  canvas.style.display = 'block';

  if (window.__appendToOutput) {
    window.__appendToOutput(canvas);
  }

  var ctx = canvas.getContext('2d');
  var dt = 1000 / 60;
  var totalFrames = Math.ceil(steps / (dt / 1000 * 60));
  var frame = 0;

  function draw() {
    ctx.clearRect(0, 0, worldWidth, worldHeight);
    bodies.forEach(function(body) {
      var verts = body.vertices;
      ctx.beginPath();
      ctx.moveTo(verts[0].x, verts[0].y);
      for (var j = 1; j < verts.length; j++) {
        ctx.lineTo(verts[j].x, verts[j].y);
      }
      ctx.closePath();
      ctx.fillStyle = body.isStatic ? '#666' : '#8cf';
      ctx.fill();
      ctx.strokeStyle = '#fff';
      ctx.lineWidth = 1;
      ctx.stroke();
    });
  }

  function step() {
    if (frame >= totalFrames) {
      draw();
      return;
    }
    Matter.Engine.update(engine, dt);
    draw();
    frame++;
    requestAnimationFrame(step);
  }

  step();
}

// Plinko: ball bouncing through a grid of pegs
function initPlinko(canvasId) {
  var canvas = document.getElementById(canvasId);
  if (!canvas) return;

  var pw = 350;
  var ph = 500;
  canvas.width = pw;
  canvas.height = ph;

  var engine = Matter.Engine.create();

  // Pegs
  var rows = 10;
  var pegRadius = 5;
  var spacingX = 30;
  var spacingY = 40;
  var startY = 60;

  var pegs = [];
  for (var row = 0; row < rows; row++) {
    var cols = row % 2 === 0 ? 10 : 9;
    var offsetX = row % 2 === 0 ? (pw - (cols - 1) * spacingX) / 2 : (pw - (cols - 1) * spacingX) / 2 + spacingX / 2;
    offsetX = row % 2 === 0 ? (pw - (cols - 1) * spacingX) / 2 : (pw - (cols - 1) * spacingX) / 2;
    // Center the grid
    var totalWidth = (cols - 1) * spacingX;
    offsetX = (pw - totalWidth) / 2;
    for (var col = 0; col < cols; col++) {
      var peg = Matter.Bodies.circle(
        offsetX + col * spacingX,
        startY + row * spacingY,
        pegRadius,
        { isStatic: true, restitution: 0.5 }
      );
      pegs.push(peg);
    }
  }

  // Walls
  var wallLeft = Matter.Bodies.rectangle(-5, ph / 2, 10, ph, { isStatic: true });
  var wallRight = Matter.Bodies.rectangle(pw + 5, ph / 2, 10, ph, { isStatic: true });
  var floor = Matter.Bodies.rectangle(pw / 2, ph + 5, pw, 10, { isStatic: true });

  // Bin dividers at the bottom
  var binDividers = [];
  for (var d = 0; d <= 10; d++) {
    var dx = (pw - 9 * spacingX) / 2 + d * spacingX - spacingX / 2;
    binDividers.push(Matter.Bodies.rectangle(dx, ph - 30, 2, 60, { isStatic: true }));
  }

  Matter.Composite.add(engine.world, pegs.concat([wallLeft, wallRight, floor]).concat(binDividers));

  // Ball
  var ballX = pw / 2 + (Math.random() - 0.5) * 10;
  var ball = Matter.Bodies.circle(ballX, 10, 8, {
    restitution: 0.5,
    friction: 0.01,
    density: 0.002
  });
  Matter.Composite.add(engine.world, [ball]);

  var ctx = canvas.getContext('2d');
  var dt = 1000 / 60;

  function draw() {
    ctx.clearRect(0, 0, pw, ph);

    // Draw pegs
    ctx.fillStyle = '#aaa';
    pegs.forEach(function(peg) {
      ctx.beginPath();
      ctx.arc(peg.position.x, peg.position.y, pegRadius, 0, Math.PI * 2);
      ctx.fill();
    });

    // Draw bin dividers
    ctx.fillStyle = '#666';
    binDividers.forEach(function(div) {
      ctx.fillRect(div.position.x - 1, div.position.y - 30, 2, 60);
    });

    // Draw ball
    ctx.fillStyle = '#f44';
    ctx.beginPath();
    ctx.arc(ball.position.x, ball.position.y, 8, 0, Math.PI * 2);
    ctx.fill();
    ctx.strokeStyle = '#fff';
    ctx.lineWidth = 1;
    ctx.stroke();
  }

  var frame = 0;
  var maxFrames = 600; // 10 seconds at 60fps

  function step() {
    if (frame >= maxFrames) {
      draw();
      return;
    }
    Matter.Engine.update(engine, dt);
    draw();
    frame++;
    requestAnimationFrame(step);
  }

  step();
}

// Setup: expose globals for ClojureScript js/ interop
function setup() {
  window.worldWidth = worldWidth;
  window.worldHeight = worldHeight;
  window.runPhysics = runPhysics;
  window.animatePhysics = animatePhysics;
  window.initPlinko = initPlinko;
}

module.exports = { worldWidth, worldHeight, runPhysics, animatePhysics, initPlinko, setup };
