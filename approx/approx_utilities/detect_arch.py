import sh
import sys
from pathlib import Path

prefix = Path(sys.argv[1])
binpath = prefix / Path('bin')

amdarch_path = binpath / Path('amdgpu-arch')

amdarch = sh.Command(amdarch_path)

try:
  arch_amd = amdarch().split()
except sh.ErrorReturnCode_1:
  arch_amd = 0

if arch_amd:
  print(f'amdgcn;{arch_amd[0]}')
  sys.exit(0)

else:
  nvptxarch_path = binpath / Path('nvptx-arch')
  nvptxarch = sh.Command(nvptxarch_path)
  arch_nvptx = nvptxarch().split()
  if arch_nvptx:
      print(f'nvptx64;{arch_nvptx[0]}')
      sys.exit(0)

sys.exit(1)
