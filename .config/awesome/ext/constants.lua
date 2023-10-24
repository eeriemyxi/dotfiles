local constants = {}

constants.MOD_KEY = "Mod4"
constants.NETWORK_INTERFACE_NAME = "wlp5s0"

constants.SH_CMD_CPU_USAGE =
    [[python -c "import psutil;print(f'{psutil.cpu_percent(interval=2):.0f}', end='')"]]
constants.SH_CMD_MEM_USAGE =
    [[python -c "import psutil;print(f'{psutil.virtual_memory().percent:.0f}', end='')"]]
constants.SH_CMD_NET_USAGE = string.format(
    [[python -c "import psutil, time; n1 = psutil.net_io_counters(pernic=True); time.sleep(1); n2 = psutil.net_io_counters(pernic=True); s = (lambda n1, n2, it, a: (getattr(n2[it], a) - getattr(n1[it], a))); print(s(n1, n2, (it := '%s'), 'bytes_sent'), s(n1, n2, it, 'bytes_recv'))"]],
    constants.NETWORK_INTERFACE_NAME
)

return constants
