<script lang="ts">
  import clsx from "clsx";
  // First implemented in commit 61c0b09cae476c41b8c117960fefd2b4da8c1c02
  import { navigating } from "$src/stores";
  import { Tween } from "svelte/motion";
  import { cubicOut } from "svelte/easing";
  const progress = new Tween(0, { duration: 3000, easing: cubicOut });
  navigating.subscribe((value) => {
    if (value) {
      progress.set(0, { duration: 0 });
      progress.set(0.6);
    } else {
      progress.set(0.6, { duration: 0 });
      progress.set(1, { duration: 1000 });
    }
  });

  function progressToOpacity(p: number) {
    const threshold = 0.7;
    const clampedValue = Math.min(1, Math.max(threshold, p));
    const rangeSize = 1 - threshold;
    return (1 - (clampedValue - threshold) / rangeSize) * 100;
  }
</script>

<div class="fixed left-0 right-0 top-0 h-1">
  <div
    class={clsx(
      "progress bg-surface-800-100-token h-1",
      $navigating && "animate-pulse",
    )}
    style="--width:{progress.current * 100}%;--opacity:{progressToOpacity(
      progress.current,
    )}%"
  ></div>
</div>

<style>
  .progress {
    width: var(--width);
    opacity: var(--opacity);
  }
</style>
