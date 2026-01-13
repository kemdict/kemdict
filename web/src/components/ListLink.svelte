<script>
  export let href;
  import clsx from "clsx";
</script>

<li>
  <a
    class={clsx(
      "block rounded transition",
      "hover:bg-surface-100 dark:hover:bg-stone-800",
      "-mx-1 my-2 p-1",
    )}
    {href}
  >
    <div class="relative">
      <h2
        class="font-bold text-surface-950 underline [text-underline-offset:4px] dark:text-surface-100"
      >
        <slot name="heading" />
      </h2>
      <slot name="afterHeading" />
    </div>
    {#if $$slots.body}
      <p class="truncate text-gray-500 dark:text-stone-300">
        <slot name="body" />
      </p>
    {/if}
    <slot name="after" />
  </a>
</li>
