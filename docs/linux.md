# Missing Decoders for AMD Old GPUs on Linux
See: <https://en.opensuse.org/SDB:AMDGPU#Hybrid_Graphics_Configuration>

Adding those flags to your bootloader might fix it. My relevant configuration
for GRUB:

```ini
GRUB_CMDLINE_LINUX_DEFAULT="splash radeon.si_support=0 amdgpu.si_support=1 radeon.cik_support=0 amdgpu.cik_support=1"
```

# Reducing Swappiness
Swap may be used unnecessarily even when you are not low on RAM. Instead of
disabling swap entirely, edit the frequency instead:

```shell
echo "vm.swappiness=10" | sudo tee -a /etc/sysctl.conf
```
