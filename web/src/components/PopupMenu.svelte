<script lang="ts">
  import { popup } from "@skeletonlabs/skeleton";
  // I don't understand why these aren't exported from
  // @skeletonlabs/skeleton.
  type Direction = "top" | "bottom" | "left" | "right";
  type Placement = Direction | `${Direction}-start` | `${Direction}-end`;

  // Appearance
  /**
   * The text for the button to open the popup.
   */
  export let label: string;

  export let btnClass = "btn";
  /**
   * Classes for the arrow connecting the popup with the button. Set
   * this to the empty string to remove the arrow.
   */
  export let arrowClass = "arrow bg-surface-100-800-token";
  /**
   * Classes for the popup itself. The popup will get a z-10 class
   * regardless of this option.
   */
  export let cardClass = "card p-4";

  // Popup settings
  export let closeQuery: string = undefined;
  /**
   * The identifier for the popup. Defaults to a random value.
   */
  export let target: string =
    "random" + Math.random().toString().slice(2, 8).padStart(6, "0");
  export let placement: Placement = "bottom";

  // Close button
  /**
   * Whether to show a close button.
   */
  export let closeBtn: boolean = false;

  const closeQueries = [];
  if (closeQuery) closeQueries.push(closeQuery);
  if (closeBtn) closeQueries.push("#close");
  let actualCloseQuery = closeQueries.join(",");
</script>

<button
  type="button"
  class={btnClass}
  use:popup={{
    event: "hover",
    placement,
    target,
    closeQuery: actualCloseQuery,
  }}>{label}</button
>
<div class="z-10 {cardClass}" data-popup={target}>
  {#if closeBtn}
    <button id="close" type="button" class="block w-full text-right">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        class="float-right inline h-6 w-6"
        viewBox="0 0 512 512"
        ><path
          fill="none"
          stroke="currentColor"
          stroke-linecap="round"
          stroke-linejoin="round"
          stroke-width="32"
          d="M368 368L144 144M368 144L144 368"
        /></svg
      >
    </button>
  {/if}
  <slot />
  {#if arrowClass !== ""}
    <div class={arrowClass} />
  {/if}
</div>
