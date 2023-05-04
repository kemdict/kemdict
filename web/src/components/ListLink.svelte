<script>
  export let href;
</script>

<li>
  <a {href}>
    <div
      class="hover:bg-surface-50 -mx-1 my-2 p-1 text-sm transition dark:hover:bg-stone-800"
    >
      <h2 class="link font-bold hover:no-underline">
        <slot name="heading" />
      </h2>
      {#if $$slots.body}
        <p class="truncate text-gray-500 dark:text-stone-300">
          <slot name="body" />
        </p>
      {/if}
    </div>
  </a>
</li>
