// Entry of the SharedWorker. See worker.ts.
import { startWorker, type WorkerScope } from "./worker";

startWorker(self as unknown as WorkerScope);
