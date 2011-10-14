#ifndef THREADED_H
#define THREADED_H
#include <stddef.h>

#include <QThread>
#include <QMutex>


#ifdef _DEBUG
#include <QElapsedTimer>
#endif

namespace Livewire
{
	class Threaded : public QThread
	{
		Q_OBJECT

	private:
		QString _name;
		QMutex _lock;
#ifdef _DEBUG
		QElapsedTimer _timer;
#endif
		bool _isExecuting;
		int _progress, _partialProgress, _inc, _totalProgress;

	protected: Threaded(char *name);
	protected: ~Threaded();

	protected:
		void SetProgress(int value);
		void SetTotalProgress(int value);
		void IncProgress();
	public:
		int GetProgress();
		int GetTotalProgress();

	protected:
		void Checkpoint(char *info);
	public:
		bool IsExecuting();

	protected:
		void Start();
		void run();
		virtual void Run() = 0;
	public:
		void Stop();

	signals:
		void ProgressChanged(int value);
	};
}

#endif
