<script lang="ts">
  let self: HTMLButtonElement;
  interface Props {
    disabled?: boolean | null | undefined;
    getstr: () => string;
    label: string;
  }
  const { disabled, getstr, label } = $props();
</script>

<button
  bind:this={self}
  {disabled}
  onclick={() => {
    navigator.clipboard.writeText(getstr());
    const oldInnerText = self.innerText;
    self.innerText = "已複製！";
    setTimeout(() => {
      self.innerText = oldInnerText;
    }, 1000);
  }}
  class="variant-filled btn">{label}</button
>
