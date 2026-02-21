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
