<script lang="ts">
  interface Props {
    id?: string | undefined;
    children?: import("svelte").Snippet;
  }

  let { id = undefined, children }: Props = $props();

  let self: HTMLButtonElement;
</script>

<button
  bind:this={self}
  title="複製"
  onclick={() => {
    navigator.clipboard.writeText(self.innerText);
    // eh...
    const oldInnerHTML = self.innerHTML;
    self.append(" - 已複製！");
    setTimeout(() => {
      self.innerHTML = oldInnerHTML;
    }, 1000);
  }}
  class="hover:opacity-75 active:opacity-90 fade cursor-pointer my-2 text-gray-800 dark:text-gray-200 text-lg"
  {id}>{@render children?.()}</button
>
