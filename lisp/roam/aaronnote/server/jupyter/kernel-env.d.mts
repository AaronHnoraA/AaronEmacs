export function buildKernelEnv(options?: {
  kernelSpecEnv?: Record<string, string>;
  venvBinDir?: string;
  pythonNoUserSite?: boolean;
}): NodeJS.ProcessEnv;
