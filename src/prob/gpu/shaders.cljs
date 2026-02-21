(ns prob.gpu.shaders
  "WGSL shader source strings for GPU tensor operations.
   Templates use clojure.string/replace for parameterized ops."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Binary op template — modulo broadcasting
;; ---------------------------------------------------------------------------

(defn- binary-op-shader [op]
  (str
   "@group(0) @binding(0) var<storage, read> a: array<f32>;
    @group(0) @binding(1) var<storage, read> b: array<f32>;
    @group(0) @binding(2) var<storage, read_write> output: array<f32>;
    @compute @workgroup_size(64)
    fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
      let idx = gid.x;
      if (idx >= arrayLength(&output)) { return; }
      let va = a[idx % arrayLength(&a)];
      let vb = b[idx % arrayLength(&b)];
      output[idx] = va " op " vb;
    }"))

(def add-shader (binary-op-shader "+"))
(def subtract-shader (binary-op-shader "-"))
(def multiply-shader (binary-op-shader "*"))
(def divide-shader (binary-op-shader "/"))

;; ---------------------------------------------------------------------------
;; Unary op template
;; ---------------------------------------------------------------------------

(defn- unary-op-shader [expr]
  (str
   "@group(0) @binding(0) var<storage, read> input: array<f32>;
    @group(0) @binding(1) var<storage, read_write> output: array<f32>;
    @compute @workgroup_size(64)
    fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
      let idx = gid.x;
      if (idx >= arrayLength(&output)) { return; }
      let x = input[idx];
      output[idx] = " expr ";
    }"))

(def negative-shader (unary-op-shader "-x"))
(def exp-shader (unary-op-shader "exp(x)"))
(def log-shader (unary-op-shader "log(x)"))
(def sqrt-shader (unary-op-shader "sqrt(x)"))
(def square-shader (unary-op-shader "x * x"))
(def abs-shader (unary-op-shader "abs(x)"))

;; ---------------------------------------------------------------------------
;; Comparison template — outputs f32 (1.0 = true, 0.0 = false)
;; ---------------------------------------------------------------------------

(defn- comparison-shader [cmp]
  (str
   "@group(0) @binding(0) var<storage, read> a: array<f32>;
    @group(0) @binding(1) var<storage, read> b: array<f32>;
    @group(0) @binding(2) var<storage, read_write> output: array<f32>;
    @compute @workgroup_size(64)
    fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
      let idx = gid.x;
      if (idx >= arrayLength(&output)) { return; }
      let va = a[idx % arrayLength(&a)];
      let vb = b[idx % arrayLength(&b)];
      output[idx] = select(0.0, 1.0, va " cmp " vb);
    }"))

(def greater-shader (comparison-shader ">"))
(def less-shader (comparison-shader "<"))
(def greater-equal-shader (comparison-shader ">="))
(def less-equal-shader (comparison-shader "<="))

;; ---------------------------------------------------------------------------
;; Where shader — 3 inputs + 1 output
;; ---------------------------------------------------------------------------

(def where-shader
  "@group(0) @binding(0) var<storage, read> cond_arr: array<f32>;
   @group(0) @binding(1) var<storage, read> a: array<f32>;
   @group(0) @binding(2) var<storage, read> b: array<f32>;
   @group(0) @binding(3) var<storage, read_write> output: array<f32>;
   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&output)) { return; }
     let c = cond_arr[idx % arrayLength(&cond_arr)];
     let va = a[idx % arrayLength(&a)];
     let vb = b[idx % arrayLength(&b)];
     output[idx] = select(vb, va, c > 0.0);
   }")

;; ---------------------------------------------------------------------------
;; Sum reduction shader — tree reduction with workgroup shared memory
;; ---------------------------------------------------------------------------

(def sum-reduction-shader
  "var<workgroup> wg_data: array<f32, 64>;

   @group(0) @binding(0) var<storage, read> input: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>,
           @builtin(local_invocation_id) lid: vec3<u32>,
           @builtin(workgroup_id) wid: vec3<u32>) {
     let global_idx = gid.x;
     let local_idx = lid.x;
     let input_len = arrayLength(&input);

     // Load element or zero if out of bounds
     if (global_idx < input_len) {
       wg_data[local_idx] = input[global_idx];
     } else {
       wg_data[local_idx] = 0.0;
     }
     workgroupBarrier();

     // Tree reduction in shared memory
     var stride: u32 = 32u;
     loop {
       if (stride == 0u) { break; }
       if (local_idx < stride) {
         wg_data[local_idx] = wg_data[local_idx] + wg_data[local_idx + stride];
       }
       workgroupBarrier();
       stride = stride >> 1u;
     }

     // First thread writes workgroup result
     if (local_idx == 0u) {
       output[wid.x] = wg_data[0];
     }
   }")

;; ---------------------------------------------------------------------------
;; Fill shader — fill buffer with a constant value
;; ---------------------------------------------------------------------------

(defn fill-shader [value]
  (str
   "@group(0) @binding(0) var<storage, read_write> output: array<f32>;
    @compute @workgroup_size(64)
    fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
      let idx = gid.x;
      if (idx >= arrayLength(&output)) { return; }
      output[idx] = " value ";
    }"))

;; ---------------------------------------------------------------------------
;; RNG shaders
;; ---------------------------------------------------------------------------

(def uniform-rng-shader
  "// PCG hash-based uniform RNG
   @group(0) @binding(0) var<storage, read> seeds: array<u32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&output)) { return; }
     var state = seeds[idx];
     // PCG hash
     state = state * 747796405u + 2891336453u;
     let word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
     let result = (word >> 22u) ^ word;
     // Convert to f32 in [0, 1)
     output[idx] = f32(result) / 4294967296.0;
   }")

(def normal-rng-shader
  "// Box-Muller transform over pairs of uniforms
   @group(0) @binding(0) var<storage, read> uniforms: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     if (idx >= arrayLength(&output)) { return; }
     // Each thread uses a pair of uniforms
     let pair_idx = idx * 2u;
     if (pair_idx + 1u >= arrayLength(&uniforms)) { return; }
     let u1 = uniforms[pair_idx];
     let u2 = uniforms[pair_idx + 1u];
     // Clamp u1 away from 0 to avoid log(0)
     let u1_safe = max(u1, 0.00001);
     let r = sqrt(-2.0 * log(u1_safe));
     let theta = 6.283185307179586 * u2;
     output[idx] = r * cos(theta);
   }")

;; ---------------------------------------------------------------------------
;; Matmul shader — tiled 16×16 workgroups
;; ---------------------------------------------------------------------------

(def matmul-shader
  "// Tiled matmul: A[M,K] × B[K,N] → C[M,N]
   // dims uniform: (M, K, N, pad)
   @group(0) @binding(0) var<storage, read> a: array<f32>;
   @group(0) @binding(1) var<storage, read> b: array<f32>;
   @group(0) @binding(2) var<storage, read_write> output: array<f32>;
   @group(0) @binding(3) var<uniform> dims: vec4<u32>;

   var<workgroup> tile_a: array<array<f32, 16>, 16>;
   var<workgroup> tile_b: array<array<f32, 16>, 16>;

   @compute @workgroup_size(16, 16)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>,
           @builtin(local_invocation_id) lid: vec3<u32>) {
     let M = dims.x;
     let K = dims.y;
     let N = dims.z;
     let row = gid.y;
     let col = gid.x;
     let lr = lid.y;
     let lc = lid.x;
     let num_tiles = (K + 15u) / 16u;
     var acc: f32 = 0.0;

     for (var t: u32 = 0u; t < num_tiles; t = t + 1u) {
       // Load tile from A
       let a_col = t * 16u + lc;
       if (row < M && a_col < K) {
         tile_a[lr][lc] = a[row * K + a_col];
       } else {
         tile_a[lr][lc] = 0.0;
       }
       // Load tile from B
       let b_row = t * 16u + lr;
       if (b_row < K && col < N) {
         tile_b[lr][lc] = b[b_row * N + col];
       } else {
         tile_b[lr][lc] = 0.0;
       }
       workgroupBarrier();

       // Accumulate dot product for this tile
       for (var k: u32 = 0u; k < 16u; k = k + 1u) {
         acc = acc + tile_a[lr][k] * tile_b[k][lc];
       }
       workgroupBarrier();
     }

     // Write result
     if (row < M && col < N) {
       output[row * N + col] = acc;
     }
   }")

;; ---------------------------------------------------------------------------
;; Transpose shader — linear 1D dispatch
;; ---------------------------------------------------------------------------

(def transpose-shader
  "// Physical transpose: input[rows, cols] → output[cols, rows]
   // dims uniform: (rows, cols, pad, pad)
   @group(0) @binding(0) var<storage, read> input: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @group(0) @binding(2) var<uniform> dims: vec4<u32>;

   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     let rows = dims.x;
     let cols = dims.y;
     let total = rows * cols;
     if (idx >= total) { return; }
     let row = idx / cols;
     let col = idx % cols;
     output[col * rows + row] = input[idx];
   }")

;; ---------------------------------------------------------------------------
;; Slice shader — inner-dimension slicing
;; ---------------------------------------------------------------------------

(def slice-shader
  "// Slice along arbitrary dimension (non-contiguous case)
   // params[0]: (slice_start, inner_size, outer_size, slice_len)
   // params[1]: (src_dim_size, pad, pad, pad)
   @group(0) @binding(0) var<storage, read> input: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @group(0) @binding(2) var<uniform> params: array<vec4<u32>, 2>;

   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     let slice_start = params[0].x;
     let inner_size = params[0].y;
     let outer_size = params[0].z;
     let slice_len = params[0].w;
     let src_dim_size = params[1].x;
     let out_total = outer_size * slice_len * inner_size;
     if (idx >= out_total) { return; }
     // Decompose output index into (outer, slice_dim, inner)
     let inner_idx = idx % inner_size;
     let tmp = idx / inner_size;
     let slice_idx = tmp % slice_len;
     let outer_idx = tmp / slice_len;
     // Compute source index
     let src_dim_idx = slice_start + slice_idx;
     let src_idx = outer_idx * (src_dim_size * inner_size) + src_dim_idx * inner_size + inner_idx;
     output[idx] = input[src_idx];
   }")

;; ---------------------------------------------------------------------------
;; Concat shader — copy-with-offset into shared output
;; ---------------------------------------------------------------------------

(def concat-shader
  "// Copy one input tensor into the correct region of the output buffer
   // params[0]: (dst_offset, src_total, inner_size, src_dim_size)
   // params[1]: (dst_dim_size, outer_size, pad, pad)
   @group(0) @binding(0) var<storage, read> input: array<f32>;
   @group(0) @binding(1) var<storage, read_write> output: array<f32>;
   @group(0) @binding(2) var<uniform> params: array<vec4<u32>, 2>;

   @compute @workgroup_size(64)
   fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
     let idx = gid.x;
     let dst_offset = params[0].x;
     let src_total = params[0].y;
     let inner_size = params[0].z;
     let src_dim_size = params[0].w;
     let dst_dim_size = params[1].x;
     let outer_size = params[1].y;
     if (idx >= src_total) { return; }
     // Decompose source index into (outer, dim, inner)
     let inner_idx = idx % inner_size;
     let tmp = idx / inner_size;
     let dim_idx = tmp % src_dim_size;
     let outer_idx = tmp / src_dim_size;
     // Map to destination with offset
     let dst_idx = outer_idx * (dst_dim_size * inner_size) + (dst_offset + dim_idx) * inner_size + inner_idx;
     output[dst_idx] = input[idx];
   }")
