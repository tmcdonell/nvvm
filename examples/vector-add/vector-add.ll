target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v16:16:16-v32:32:32-v64:64:64-v128:128:128-n16:32:64"
target triple = "nvptx64-nvidia-cuda"

define void @kernel(float* %A, float* %B, float* %C) {
entry:
  ; What is my ID?
  %id = tail call i32 @llvm.nvvm.read.ptx.sreg.tid.x() readnone nounwind

  ; Compute pointers into A, B, and C
  %ptrA = getelementptr float* %A, i32 %id
  %ptrB = getelementptr float* %B, i32 %id
  %ptrC = getelementptr float* %C, i32 %id

  ; Read A, B
  %valA = load float* %ptrA, align 4
  %valB = load float* %ptrB, align 4

  ; Compute C = A + B
  %valC = fadd float %valA, %valB

  ; Store back to C
  store float %valC, float* %ptrC, align 4

  ret void
}

; Intrinsic to read threadIdx.x
declare i32 @llvm.nvvm.read.ptx.sreg.tid.x() readnone nounwind

!nvvm.annotations = !{!0}
!0 = metadata !{void (float*, float*, float*)* @kernel, metadata !"kernel", i64 1}

