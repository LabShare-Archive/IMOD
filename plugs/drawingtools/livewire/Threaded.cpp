#include "Threaded.h"

using namespace Livewire;

Threaded::Threaded(char *name) : _name(name), _isExecuting(false), _progress(0), _partialProgress(0), _inc(1), _totalProgress(100) {}
Threaded::~Threaded() { }
int Threaded::GetProgress() { return this->_progress; }
void Threaded::SetProgress(int value)
{
	if (this->_progress != value)
	{
		this->_partialProgress += value - this->_progress;
		this->_progress = value;
		if (this->_partialProgress >= this->_inc)
		{
			this->_partialProgress %= this->_inc;
			emit ProgressChanged(this->_progress);
		}
	}
}
int Threaded::GetTotalProgress() { return this->_totalProgress; }
void Threaded::SetTotalProgress(int value) { this->_totalProgress = value; this->_inc = this->_totalProgress / 100; }
void Threaded::IncProgress()
{
	++this->_progress;
	if (++this->_partialProgress == this->_inc)
	{
		this->_partialProgress = 0;
		emit ProgressChanged(this->_progress);
	}
}

bool Threaded::IsExecuting() { return this->_isExecuting; }
void Threaded::Checkpoint(char *info)
{
#ifdef _DEBUG
	qDebug("%s thread took %f secs to get to %s", this->_name.toAscii().data(), this->_timer.elapsed() / 1000.0, info);
#else
	(info); // unreferenced parameter
#endif
}

void Threaded::Start()
{
	this->Stop();

	this->_lock.lock();
	this->_isExecuting = true;
	this->_lock.unlock();

	this->_progress = 0;

	this->start();
}
void Threaded::run()
{
#ifdef _DEBUG
	this->_timer.start();
#endif
	this->Run();
	this->_isExecuting = false;
#ifdef _DEBUG
	qDebug("%s thread took %f secs to complete", this->_name.toAscii().data(), this->_timer.elapsed() / 1000.0);
#endif
}
void Threaded::Stop()
{
	this->_lock.lock();
	if (this->_isExecuting)
	{
		this->_isExecuting = false;
		this->wait();
	}
	this->_lock.unlock();
}
